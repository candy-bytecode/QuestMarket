;; QuestMarket - Decentralized Gig Marketplace for Gamers
;; A comprehensive marketplace where gamers can post and complete tasks with escrow protection

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-QUEST-NOT-FOUND (err u404))
(define-constant ERR-INSUFFICIENT-FUNDS (err u402))
(define-constant ERR-QUEST-NOT-ACTIVE (err u403))
(define-constant ERR-ALREADY-APPLIED (err u405))
(define-constant ERR-INVALID-PARAMETERS (err u406))
(define-constant ERR-DEADLINE-PASSED (err u407))
(define-constant ERR-USER-NOT-FOUND (err u408))
(define-constant ERR-INVALID-RATING (err u409))
(define-constant ERR-DISPUTE-NOT-FOUND (err u410))
(define-constant ERR-ALREADY-RATED (err u411))

;; Data Variables
(define-data-var next-quest-id uint u1)
(define-data-var next-dispute-id uint u1)
(define-data-var platform-fee uint u250) ;; 2.5%
(define-data-var min-quest-reward uint u1000) ;; Minimum 1000 microSTX
(define-data-var max-quest-duration uint u52560) ;; Max 1 year in blocks
(define-data-var dispute-window uint u1440) ;; 1 day in blocks

;; Data Maps
(define-map quests 
  uint 
  {
    creator: principal,
    title: (string-ascii 100),
    description: (string-ascii 500),
    reward: uint,
    deadline: uint,
    status: (string-ascii 20),
    assignee: (optional principal),
    created-at: uint,
    category: (string-ascii 50),
    difficulty: uint,
    completion-proof: (optional (string-ascii 200))
  })

(define-map applications 
  {quest-id: uint, applicant: principal}
  {applied-at: uint, message: (string-ascii 200), status: (string-ascii 20)})

(define-map escrow
  uint
  {amount: uint, released: bool, dispute-deadline: uint})

(define-map user-profiles
  principal
  {
    username: (string-ascii 50),
    bio: (string-ascii 300),
    total-quests-created: uint,
    total-quests-completed: uint,
    rating: uint,
    rating-count: uint,
    created-at: uint,
    is-verified: bool
  })

(define-map user-ratings
  {rater: principal, ratee: principal, quest-id: uint}
  {rating: uint, comment: (string-ascii 200), created-at: uint})

(define-map disputes
  uint
  {
    quest-id: uint,
    initiator: principal,
    reason: (string-ascii 300),
    status: (string-ascii 20),
    created-at: uint,
    resolved-at: (optional uint),
    resolution: (optional (string-ascii 300))
  })

(define-map quest-categories
  (string-ascii 50)
  {active: bool, quest-count: uint})

;; User Profile Management Functions

;; Create user profile
(define-public (create-profile (username (string-ascii 50)) (bio (string-ascii 300)))
  (begin
    (asserts! (> (len username) u0) ERR-INVALID-PARAMETERS)
    (asserts! (is-none (map-get? user-profiles tx-sender)) ERR-NOT-AUTHORIZED)
    (map-set user-profiles tx-sender {
      username: username,
      bio: bio,
      total-quests-created: u0,
      total-quests-completed: u0,
      rating: u0,
      rating-count: u0,
      created-at: stacks-block-height,
      is-verified: false
    })
    (ok true)))

;; Update user profile
(define-public (update-profile (username (string-ascii 50)) (bio (string-ascii 300)))
  (let ((profile (unwrap! (map-get? user-profiles tx-sender) ERR-USER-NOT-FOUND)))
    (asserts! (> (len username) u0) ERR-INVALID-PARAMETERS)
    (map-set user-profiles tx-sender (merge profile {
      username: username,
      bio: bio
    }))
    (ok true)))

;; Rate a user after quest completion
(define-public (rate-user (ratee principal) (quest-id uint) (rating uint) (comment (string-ascii 200)))
  (let (
    (quest (unwrap! (map-get? quests quest-id) ERR-QUEST-NOT-FOUND))
    (profile (unwrap! (map-get? user-profiles ratee) ERR-USER-NOT-FOUND))
  )
    (asserts! (is-eq (get status quest) "completed") ERR-QUEST-NOT-ACTIVE)
    (asserts! (and (>= rating u1) (<= rating u5)) ERR-INVALID-RATING)
    (asserts! (or (is-eq tx-sender (get creator quest)) 
                  (is-eq tx-sender (unwrap! (get assignee quest) ERR-NOT-AUTHORIZED))) ERR-NOT-AUTHORIZED)
    (asserts! (is-none (map-get? user-ratings {rater: tx-sender, ratee: ratee, quest-id: quest-id})) ERR-ALREADY-RATED)
    
    (map-set user-ratings {rater: tx-sender, ratee: ratee, quest-id: quest-id} {
      rating: rating,
      comment: comment,
      created-at: stacks-block-height
    })
    
    ;; Update user's average rating
    (let (
      (current-rating (get rating profile))
      (rating-count (get rating-count profile))
      (new-count (+ rating-count u1))
      (new-rating (/ (+ (* current-rating rating-count) rating) new-count))
    )
      (map-set user-profiles ratee (merge profile {
        rating: new-rating,
        rating-count: new-count
      }))
    )
    
    (ok true)))

;; Quest Creation and Management Functions

;; Create a new quest with escrow
(define-public (create-quest (title (string-ascii 100)) (description (string-ascii 500)) 
                           (reward uint) (deadline uint) (category (string-ascii 50)) (difficulty uint))
  (let ((quest-id (var-get next-quest-id)))
    (asserts! (> reward (var-get min-quest-reward)) ERR-INSUFFICIENT-FUNDS)
    (asserts! (> deadline stacks-block-height) ERR-INVALID-PARAMETERS)
    (asserts! (< (- deadline stacks-block-height) (var-get max-quest-duration)) ERR-INVALID-PARAMETERS)
    (asserts! (and (>= difficulty u1) (<= difficulty u5)) ERR-INVALID-PARAMETERS)
    (asserts! (> (len title) u0) ERR-INVALID-PARAMETERS)
    
    (try! (stx-transfer? reward tx-sender (as-contract tx-sender)))
    
    (map-set quests quest-id {
      creator: tx-sender,
      title: title,
      description: description,
      reward: reward,
      deadline: deadline,
      status: "active",
      assignee: none,
      created-at: stacks-block-height,
      category: category,
      difficulty: difficulty,
      completion-proof: none
    })
    
    (map-set escrow quest-id {
      amount: reward, 
      released: false,
      dispute-deadline: (+ deadline (var-get dispute-window))
    })
    
    ;; Update category count
    (map-set quest-categories category {
      active: true,
      quest-count: (+ (default-to u0 (get quest-count (map-get? quest-categories category))) u1)
    })
    
    ;; Update user profile
    (match (map-get? user-profiles tx-sender)
      profile (map-set user-profiles tx-sender 
                      (merge profile {total-quests-created: (+ (get total-quests-created profile) u1)}))
      true)
    
    (var-set next-quest-id (+ quest-id u1))
    (ok quest-id)))

;; Apply for a quest
(define-public (apply-for-quest (quest-id uint) (message (string-ascii 200)))
  (let ((quest (unwrap! (map-get? quests quest-id) ERR-QUEST-NOT-FOUND)))
    (asserts! (is-eq (get status quest) "active") ERR-QUEST-NOT-ACTIVE)
    (asserts! (< stacks-block-height (get deadline quest)) ERR-DEADLINE-PASSED)
    (asserts! (is-none (map-get? applications {quest-id: quest-id, applicant: tx-sender})) ERR-ALREADY-APPLIED)
    (asserts! (not (is-eq tx-sender (get creator quest))) ERR-NOT-AUTHORIZED)
    
    (map-set applications {quest-id: quest-id, applicant: tx-sender} {
      applied-at: stacks-block-height,
      message: message,
      status: "pending"
    })
    (ok true)))

;; Assign quest to an applicant
(define-public (assign-quest (quest-id uint) (assignee principal))
  (let ((quest (unwrap! (map-get? quests quest-id) ERR-QUEST-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get creator quest)) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status quest) "active") ERR-QUEST-NOT-ACTIVE)
    (asserts! (is-some (map-get? applications {quest-id: quest-id, applicant: assignee})) ERR-NOT-AUTHORIZED)
    
    (map-set quests quest-id (merge quest {
      status: "assigned",
      assignee: (some assignee)
    }))
    
    ;; Update application status
    (match (map-get? applications {quest-id: quest-id, applicant: assignee})
      app (map-set applications {quest-id: quest-id, applicant: assignee}
                   (merge app {status: "accepted"}))
      true)
    
    (ok true)))

;; Cancel quest and refund
(define-public (cancel-quest (quest-id uint))
  (let (
    (quest (unwrap! (map-get? quests quest-id) ERR-QUEST-NOT-FOUND))
    (escrow-data (unwrap! (map-get? escrow quest-id) ERR-QUEST-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender (get creator quest)) ERR-NOT-AUTHORIZED)
    (asserts! (or (is-eq (get status quest) "active") (is-eq (get status quest) "assigned")) ERR-QUEST-NOT-ACTIVE)
    (asserts! (not (get released escrow-data)) ERR-NOT-AUTHORIZED)
    
    (try! (as-contract (stx-transfer? (get amount escrow-data) tx-sender (get creator quest))))
    (map-set quests quest-id (merge quest {status: "cancelled"}))
    (map-set escrow quest-id (merge escrow-data {released: true}))
    (ok true)))

;; Quest Completion and Payment Functions

;; Submit completion proof
(define-public (submit-completion (quest-id uint) (proof (string-ascii 200)))
  (let ((quest (unwrap! (map-get? quests quest-id) ERR-QUEST-NOT-FOUND)))
    (asserts! (is-eq tx-sender (unwrap! (get assignee quest) ERR-NOT-AUTHORIZED)) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status quest) "assigned") ERR-QUEST-NOT-ACTIVE)
    
    (map-set quests quest-id (merge quest {
      status: "submitted",
      completion-proof: (some proof)
    }))
    (ok true)))

;; Complete quest and release payment
(define-public (complete-quest (quest-id uint))
  (let (
    (quest (unwrap! (map-get? quests quest-id) ERR-QUEST-NOT-FOUND))
    (escrow-data (unwrap! (map-get? escrow quest-id) ERR-QUEST-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender (get creator quest)) ERR-NOT-AUTHORIZED)
    (asserts! (or (is-eq (get status quest) "assigned") (is-eq (get status quest) "submitted")) ERR-QUEST-NOT-ACTIVE)
    (asserts! (not (get released escrow-data)) ERR-NOT-AUTHORIZED)
    
    (let (
      (reward (get amount escrow-data))
      (fee (/ (* reward (var-get platform-fee)) u10000))
      (payout (- reward fee))
      (assignee (unwrap! (get assignee quest) ERR-NOT-AUTHORIZED))
    )
      (try! (as-contract (stx-transfer? payout tx-sender assignee)))
      (try! (as-contract (stx-transfer? fee tx-sender CONTRACT-OWNER)))
      
      (map-set quests quest-id (merge quest {status: "completed"}))
      (map-set escrow quest-id (merge escrow-data {released: true}))
      
      ;; Update assignee profile
      (match (map-get? user-profiles assignee)
        profile (map-set user-profiles assignee 
                        (merge profile {total-quests-completed: (+ (get total-quests-completed profile) u1)}))
        true)
      
      (ok true))))