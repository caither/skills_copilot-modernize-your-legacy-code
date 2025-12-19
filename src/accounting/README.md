# Account Management System - Node.js Implementation

This directory contains a Node.js implementation of the Student Account Management System, originally written in COBOL, along with comprehensive Jest unit tests.

## Overview

This implementation provides two approaches to the account management system:

1. **index.js** - Direct port from COBOL with DataProgram, Operations, and MainProgram classes
2. **AccountManager.js** - Alternative implementation with simplified API

Both implementations provide the core functionality for managing student accounts:
- **View Balance**: Check the current account balance
- **Credit Account**: Deposit money into the account
- **Debit Account**: Withdraw money from the account

## Files

### Core Application
- `index.js` - Main application (direct COBOL port with readline-sync interface)
- `AccountManager.js` - Alternative implementation with simplified API

### Test Files
- `index.test.js` - Jest tests for index.js (30 test cases)
- `AccountManager.test.js` - Jest tests for AccountManager.js (42 test cases)

### Configuration
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

This will install:
- **Jest** - Testing framework for unit tests
- **readline-sync** - For interactive CLI input (used by index.js)

## Running the Application

To run the interactive CLI application:

```bash
npm start
```

This will start the menu-driven interface matching the COBOL application.

## Running Tests

To run all Jest unit tests:

```bash
npm test
```

For test coverage:

```bash
npm run test:coverage
```

## Test Coverage

The test suite includes 72 comprehensive test cases across two implementations:

### index.test.js (30 tests)
Tests for the DataProgram and Operations classes:
- **DataProgram Tests**: Initial balance, credit/debit operations, decimal precision, multiple operations
- **Operations Tests**: Balance formatting, COBOL output format compliance
- **Interactive Menu Simulation**: Complete workflow tests
- **Business Logic Validation**: Overdraft protection, precision handling
- **COBOL Legacy Equivalence**: Read/Write operations, format matching

### AccountManager.test.js (42 tests)
Tests mirroring scenarios from `docs/TESTPLAN.md`:
- **Functional Tests**: View balance, credit, debit operations
- **Boundary Value Tests**: Min/max amounts, edge cases
- **Error Handling Tests**: Invalid inputs, precision
- **Integration Tests**: Complete workflows, system operations

## Usage Examples

### Using index.js (COBOL-style)

```javascript
const { DataProgram, Operations } = require('./index');

// Create instances
const dataProgram = new DataProgram();
const operations = new Operations(dataProgram);

// View balance
operations.total(); // Outputs: Current balance: 001000.00

// Credit account
dataProgram.write(dataProgram.read() + 500.00);
operations.total(); // Outputs: Current balance: 001500.00

// Debit account (with overdraft protection)
const balance = dataProgram.read();
if (balance >= 200.00) {
  dataProgram.write(balance - 200.00);
}
operations.total(); // Outputs: Current balance: 001300.00
```

### Using AccountManager.js (Simplified API)

```javascript
const AccountManager = require('./AccountManager');

// Create a new account
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
4. **COBOL Format Output**: Balance displayed in 9-character format (e.g., 001000.00)
5. **Interactive CLI**: Menu-driven interface matching the COBOL application
6. **Transaction Atomicity**: All operations are complete and atomic
7. **Data Consistency**: Balance queries always return the current accurate balance

## Business Rules

- Initial account balance: **1000.00**
- Maximum balance: **999999.99** (COBOL PIC 9(6)V99 limit)
- Minimum transaction amount: **0.01**
- All amounts are stored with **2 decimal places**
- Overdrafts are **not permitted**
- Failed transactions **do not affect** the account balance

## Test Results

All 72 tests pass successfully:

```
Test Suites: 2 passed, 2 total
Tests:       72 passed, 72 total
Snapshots:   0 total
Time:        0.541 s
```

## COBOL to Node.js Mapping

| COBOL Program | Node.js Equivalent | Description |
|--------------|-------------------|-------------|
| DataProgram | DataProgram class (index.js) | Manages storage balance |
| Operations | Operations class (index.js) | Handles credit/debit operations and formatting |
| MainProgram | MainProgram class (index.js) | Menu-driven CLI interface |
| - | AccountManager class | Alternative simplified implementation |

## Architecture

### index.js Structure (Direct COBOL Port)
```
MainProgram
  ├── DataProgram (data layer)
  │   ├── read() - Retrieves balance
  │   └── write() - Updates balance
  └── Operations (business logic)
      ├── total() - View balance
      ├── credit() - Add to balance
      ├── debit() - Subtract from balance
      └── formatBalance() - COBOL format output
```

### AccountManager.js Structure (Simplified)
```
AccountManager
  ├── viewBalance() - Returns current balance
  ├── creditAccount(amount) - Adds amount with validation
  ├── debitAccount(amount) - Subtracts amount with overdraft protection
  ├── formatBalance(balance) - Formats to COBOL output
  └── resetBalance() - Resets to initial or specified balance
```

## Related Documentation

- [Test Plan](../../docs/TESTPLAN.md) - Complete test plan with 40 test scenarios
- [COBOL Source](../cobol/) - Original COBOL implementation

## License

MIT
