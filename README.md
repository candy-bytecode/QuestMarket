# QuestMarket - Decentralized Gig Marketplace for Gamers

A decentralized marketplace where gamers can post and complete tasks like leveling up characters, coaching, or creating mods. Smart contracts ensure fair payment through escrow protection.

## Features

- Create gaming quests with automatic escrow
- Apply for available quests
- Quest assignment system
- Automatic payment release upon completion
- Platform fee collection (2.5%)
- Quest cancellation with refunds

## Contract Functions

### Public Functions
- `create-quest` - Create a new quest with escrow deposit
- `apply-for-quest` - Apply for an available quest
- `assign-quest` - Assign quest to chosen applicant
- `complete-quest` - Mark quest complete and release payment
- `cancel-quest` - Cancel quest and refund creator

### Read-Only Functions
- `get-quest` - Get quest details by ID
- `get-application` - Get application details
- `get-platform-fee` - Current platform fee percentage

## Quest Lifecycle

1. Creator posts quest with STX escrow
2. Gamers apply with application messages
3. Creator assigns quest to chosen applicant
4. Upon completion verification, payment is released
5. Platform fee is deducted automatically

## Usage

Deploy the contract and interact through Stacks wallet or dApp interface.