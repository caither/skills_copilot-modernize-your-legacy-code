# Account Management System

This is a Node.js application modernized from a COBOL legacy system. It provides account management functionality with the following features:

## Features

- **View Balance** - Display current account balance
- **Credit Account** - Add funds to the account (deposit)
- **Debit Account** - Withdraw funds from the account
- **Overdraft Protection** - Prevents balance from going negative
- **Exit** - Close the application

## Business Rules (Preserved from COBOL)

- Initial account balance: **1000.00**
- All amounts are stored with **2 decimal precision**
- **Overdraft protection**: Debit operations are rejected if insufficient funds
- Balance range: **0.00 to 999,999.99**

## Prerequisites

- Node.js (v14 or higher)
- npm (Node Package Manager)

## Installation

1. Change to the accounting directory:
   ```bash
   cd src/accounting
   ```

2. Install dependencies:
   ```bash
   npm install
   ```

## Running the Application

### Method 1: Using npm script
```bash
cd src/accounting
npm start
```

### Method 2: Direct execution
```bash
cd src/accounting
node index.js
```

### Method 3: Using VS Code Debugger
1. Open the project in VS Code
2. Go to Run and Debug (Ctrl+Shift+D)
3. Select "Launch Accounting Application"
4. Press F5 or click the green play button

## Testing

Run the automated test suite:
```bash
cd src/accounting
node test.js
```

Run the interactive simulation test:
```bash
cd src/accounting
node test-interactive.js
```

## Usage Example

```
--------------------------------
Account Management System
1. View Balance
2. Credit Account
3. Debit Account
4. Exit
--------------------------------
Enter your choice (1-4): 1
Current balance: 001000.00

Enter your choice (1-4): 2
Enter credit amount: 500.00
Amount credited. New balance: 001500.00

Enter your choice (1-4): 3
Enter debit amount: 200.00
Amount debited. New balance: 001300.00

Enter your choice (1-4): 4
Exiting the program. Goodbye!
```

## Architecture

The application has been refactored to use a **single source of business logic** with a clean separation of concerns:

### **Core Library: AccountManager** (Business Logic)
- **Location**: `AccountManager.js`
- **Responsibility**: Pure business logic (viewBalance, creditAccount, debitAccount, formatBalance)
- **Design**: Stateless methods, structured return values (success, message, balance)
- **Reusability**: Can be imported and used by any client (CLI, API, tests)
- **No Dependencies**: Does not depend on readline-sync or console I/O

### **CLI Interface: MainProgram** (User Interface)
- **Location**: `index.js`
- **Responsibility**: Menu-driven CLI interface
- **Design**: Directly uses `AccountManager` for all business logic
- **Features**: User input validation, interactive menu, formatted output

### **Legacy Adapters** (Backward Compatibility)
- **DataProgram** & **Operations** classes in `index.js` act as thin wrappers that delegate to `AccountManager`
- **Purpose**: Maintain test compatibility with existing test suite
- **Implementation**: All logic is delegated to the library; these are non-functional pass-throughs

## Data Flow

```
┌─────────────────────┐
│   User Input        │
│   (CLI Prompt)      │
└──────────┬──────────┘
           │
           ↓
┌──────────────────────────────┐
│   MainProgram (index.js)     │  ← UI/CLI Layer
│   - Display Menu             │
│   - Get User Input           │
│   - Validate Amount Input    │
└──────────┬───────────────────┘
           │
           ↓ delegates
┌──────────────────────────────┐
│   AccountManager Library     │  ← Business Logic Layer
│   - viewBalance()            │
│   - creditAccount(amount)    │
│   - debitAccount(amount)     │
│   - formatBalance(amount)    │
└──────────┬───────────────────┘
           │
           ↓
┌──────────────────────────────┐
│   In-Memory Storage          │  ← Data Layer
│   - this.balance             │
└──────────────────────────────┘
```

## Module Structure

| File | Class | Role | Used By |
|------|-------|------|---------|
| `AccountManager.js` | AccountManager | Pure business logic library | MainProgram, Tests, DataProgram (adapter), Operations (adapter) |
| `index.js` | MainProgram | CLI menu-driven interface | Entry point (`npm start`) |
| `index.js` | DataProgram | Legacy adapter for tests | Test suite compatibility |
| `index.js` | Operations | Legacy adapter for tests | Test suite compatibility |

## Migration Notes

This Node.js application is a modernized version of the COBOL system with:

### Architecture Equivalence
| COBOL Program | COBOL Purpose | Node.js Implementation | Role |
|---|---|---|---|
| main.cob | Menu UI & flow control | MainProgram class | CLI entry point using AccountManager |
| operations.cob | Business logic operations | AccountManager class | Pure library with no I/O dependencies |
| data.cob | Data storage | AccountManager.balance | Property in library instance |

### Key Modernizations
1. **Single Source of Truth**: All business logic consolidated in `AccountManager.js`
2. **Separation of Concerns**: UI logic completely separated from business logic
3. **Reusability**: AccountManager can be imported by any client (CLI, API, tests)
4. **Testability**: Pure functions with no external dependencies make testing simpler
5. **Backward Compatibility**: Thin adapters preserve existing test interfaces

### Preserved Business Rules
- ✅ Initial balance: 1000.00
- ✅ 2 decimal precision for all amounts
- ✅ Overdraft protection (no negative balances)
- ✅ Balance range: 0.00 to 999,999.99
- ✅ COBOL-style formatted output (6-digit padded whole + 2 decimals)

## License

MIT
