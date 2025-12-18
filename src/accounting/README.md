# Accounting System - Node.js Implementation

This directory contains a Node.js implementation of the Student Account Management System, originally written in COBOL, along with comprehensive unit tests.

## Overview

The AccountManager module provides the core functionality for managing student accounts:
- **View Balance**: Check the current account balance
- **Credit Account**: Deposit money into the account
- **Debit Account**: Withdraw money from the account

## Files

- `AccountManager.js` - Core business logic module
- `AccountManager.test.js` - Comprehensive unit tests (42 test cases)
- `package.json` - Node.js project configuration
- `package-lock.json` - Dependency lock file

## Prerequisites

- Node.js (version 12 or higher recommended)
- npm (comes with Node.js)

## Installation

Install the required dependencies:

```bash
npm install
```

This will install Jest, the testing framework used for unit tests.

## Running Tests

To run all unit tests:

```bash
npm test
```

For verbose output:

```bash
npm test -- --verbose
```

For test coverage:

```bash
npm test -- --coverage
```

## Test Coverage

The test suite includes 42 comprehensive test cases that mirror the scenarios documented in `docs/TESTPLAN.md`:

### Functional Tests (13 tests)
- **View Balance**: 3 tests (TC-001 to TC-003)
- **Credit/Deposit**: 5 tests (TC-004 to TC-008)
- **Debit/Withdrawal Success**: 5 tests (TC-009 to TC-013)
- **Debit/Withdrawal Failure**: 4 tests (TC-014 to TC-017)

### Boundary Value Tests (8 tests)
- **Amount Boundaries**: 6 tests (TC-018 to TC-023)
- **System State Boundaries**: 2 tests (TC-024 to TC-025)

### Error Handling Tests (5 tests)
- **Invalid Input**: 2 tests (TC-030 to TC-031)
- **Data Precision**: 3 tests (TC-032 to TC-034)

### Integration Tests (9 tests)
- **Complete Workflows**: 3 tests (TC-035 to TC-037)
- **System Operations**: 2 tests (TC-038, TC-040)
- **Additional Validation**: 7 extra tests

## Usage Example

```javascript
const AccountManager = require('./AccountManager');

// Create a new account with initial balance of 1000.00
const account = new AccountManager();

// View balance
console.log(account.viewBalance()); // 1000.00

// Credit account
const creditResult = account.creditAccount(500);
console.log(creditResult.message); // "Amount credited. New balance: 001500.00"

// Debit account
const debitResult = account.debitAccount(200);
console.log(debitResult.message); // "Amount debited. New balance: 001300.00"

// Try to overdraft (will fail)
const overdraft = account.debitAccount(2000);
console.log(overdraft.message); // "Insufficient funds for this debit."
```

## Key Features

1. **Prevents Overdraft**: The system ensures that withdrawals cannot exceed the current balance
2. **Initial Balance**: New accounts automatically start with 1000.00
3. **Precision Control**: All amounts are maintained with 2 decimal places
4. **Transaction Atomicity**: All operations are complete and atomic
5. **Data Consistency**: Balance queries always return the current accurate balance

## Business Rules

- Initial account balance: **1000.00**
- Minimum transaction amount: **0.01**
- All amounts are stored with **2 decimal places**
- Overdrafts are **not permitted**
- Failed transactions **do not affect** the account balance

## Test Results

All 42 tests pass successfully:

```
Test Suites: 1 passed, 1 total
Tests:       42 passed, 42 total
Snapshots:   0 total
Time:        0.425 s
```

## COBOL to Node.js Mapping

| COBOL Program | Node.js Equivalent | Description |
|--------------|-------------------|-------------|
| DataProgram | AccountManager.balance | Manages storage balance |
| Operations | AccountManager methods | Handles credit/debit operations |
| MainProgram | (not implemented) | Menu-driven CLI interface |

## Future Enhancements

Potential improvements for this implementation:

1. Add a CLI interface matching the COBOL MainProgram menu system
2. Implement persistent storage (file or database)
3. Add transaction history logging
4. Support multiple accounts
5. Add interest calculation features
6. Implement account types (checking, savings, etc.)

## Related Documentation

- [Test Plan](../../docs/TESTPLAN.md) - Complete test plan with 40 test scenarios
- [COBOL Source](../cobol/) - Original COBOL implementation

## License

ISC
