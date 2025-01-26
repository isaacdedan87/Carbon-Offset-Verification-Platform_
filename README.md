# Carbon Offset Verification Platform

A decentralized platform for verifying and trading carbon offsets built on Stacks blockchain using Clarity smart contracts.

## Features

- Carbon offset registration and verification
- Fungible token implementation for carbon credits
- Marketplace for trading verified carbon credits
- Automated testing pipeline

## Smart Contracts

### Carbon Verification Contract
- Register new carbon offsets
- Verify offsets by authorized parties
- Query offset information
- Track total offsets
- Mint and transfer carbon credit tokens
- Create and manage marketplace listings

## Testing

Tests are implemented using Vitest and cover:
- Offset registration
- Verification flows
- Token operations
- Marketplace functionality

Run tests:

`npm run test`

## CI/CD

GitHub Actions workflow runs:
- Contract syntax validation
- Unit tests
- Node.js environment setup

## Getting Started

1. Install dependencies:

`npm ci`

2. Run contract checks:

`clarinet check`

3. Execute tests:

`npm test`

## Tech Stack

- Clarity smart contracts
- Stacks blockchain
- Clarinet development tools
- Vitest testing framework
- GitHub Actions

## License

MIT
