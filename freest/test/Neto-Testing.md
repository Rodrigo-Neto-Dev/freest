# Neto-Testing: FreeST Affine Channels Test Suite

## Overview

This document describes the comprehensive test suite added for testing FreeST's affine channels feature (`**!T` / `**?T`). The test suite covers program tests (valid and invalid), unit tests for type system rules, and property-based tests for semantic verification.

---

## Directory Structure

```
freest/test/
├── prog/
│   ├── Valid/
│   │   └── AffineChannels/
│   │   ├── SingleProducerTermination/
│   │   ├── MultiProducerClone/
│   │   ├── DropOnlyTermination/
│   │   ├── RecursiveSend/
│   │   ├── InterleavingSumCheck/
│   │   ├── LargeCloneFan/
│   │   ├── RecursiveCloneProducers/
│   │   └── ExactlyOneNothing/
│   └── Invalid/
│       └── AffineChannels/
│           ├── MissingDrop/
│           ├── DoubleDrop/
│           ├── UseAfterDrop/
│           ├── ImplicitDuplication/
│           ├── ReceiverDuplication/
│           ├── CloneReceiver/
│           ├── BranchMissingDrop/
│           ├── ImplicitReuse/
│           └── SenderBoundTwice/
├── unit/
│   ├── AffineWellFormedness.test
│   ├── AffineConsumption.test
│   ├── AffineCloneRules.test
│   ├── AffineDropRules.test
│   ├── AffineBranching.test
│   ├── AffineRecursion.test
│   ├── AffineHigherOrder.test
│   ├── AffineLinearInteraction.test
│   └── AffineReceiverLinearity.test
└── property/
    └── AffineChannelsProperties.hs
```

---

## Program Tests (Valid)

### 1. SingleProducerTermination

**File:** `Valid/AffineChannels/SingleProducerTermination/`

Tests basic single-producer scenario with explicit termination detection. Sends three values, drops the sender, and verifies the receiver gets all values followed by the termination signal (`Nothing`).

**Expected Output:**
```
1
2
3
done
```

### 2. MultiProducerClone

**File:** `Valid/AffineChannels/MultiProducerClone/`

Tests cloning a sender to create multiple producers. Two cloned senders each send one value before being dropped.

**Expected Output:**
```
10
20
```

### 3. DropOnlyTermination

**File:** `Valid/AffineChannels/DropOnlyTermination/`

Tests immediate channel closure with no messages. Verifies that dropping the sender without sending produces immediate termination.

**Expected Output:**
```
closed
```

### 4. RecursiveSend

**File:** `Valid/AffineChannels/RecursiveSend/`

Tests recursive sending with countdown pattern. The `sendDown` function recursively sends values from n down to 1.

**Expected Output:**
```
5
4
3
2
1
```

### 5. InterleavingSumCheck

**File:** `Valid/AffineChannels/InterleavingSumCheck/`

Tests concurrent producers with forked threads. Three producers send values (1, 5, 9) via cloned senders. Order is nondeterministic but sum must be 15.

**Expected Output:**
```
15
```

### 6. LargeCloneFan

**File:** `Valid/AffineChannels/LargeCloneFan/`

Tests nested clone fan-out pattern with four producers created through chained cloning. Verifies sum equals 10 (1+2+3+4).

**Expected Output:**
```
10
```

### 7. RecursiveCloneProducers

**File:** `Valid/AffineChannels/RecursiveCloneProducers/`

Tests recursive cloning where each level clones and sends one value. Four levels send values 1, 2, 3, 4.

**Expected Output:**
```
1
2
3
4
```

### 8. ExactlyOneNothing

**File:** `Valid/AffineChannels/ExactlyOneNothing/`

Tests termination counting. Verifies exactly one `Nothing` is received after all senders are dropped.

**Expected Output:**
```
1
```

---

## Program Tests (Invalid)

These programs should be **rejected** by the type checker.

### 1. MissingDrop

**File:** `Invalid/AffineChannels/MissingDrop/`

```fst
main =
  let (s, r) = newA in
  sendA 1 s  -- Error: s not dropped
```

**Error:** Sender escapes scope without being consumed.

### 2. DoubleDrop

**File:** `Invalid/AffineChannels/DoubleDrop/`

```fst
main =
  let (s, r) = newA in
  drop s;
  drop s  -- Error: double drop
```

**Error:** Same sender dropped twice.

### 3. UseAfterDrop

**File:** `Invalid/AffineChannels/UseAfterDrop/`

```fst
main =
  let (s, r) = newA in
  drop s;
  sendA 99 s  -- Error: use after drop
```

**Error:** Sender used after being dropped.

### 4. ImplicitDuplication

**File:** `Invalid/AffineChannels/ImplicitDuplication/`

```fst
main =
  let (s, r) = newA in
  consume s;
  consume s  -- Error: implicit duplication
```

**Error:** Sender passed to function twice without cloning.

### 5. ReceiverDuplication

**File:** `Invalid/AffineChannels/ReceiverDuplication/`

```fst
main =
  let (s, r) = newA in
  drop s;
  consume r;
  consume r  -- Error: receiver duplication
```

**Error:** Receiver passed to function twice.

### 6. CloneReceiver

**File:** `Invalid/AffineChannels/CloneReceiver/`

```fst
main =
  let (s, r) = newA in
  drop s;
  let (r1, r2) = clone r in  -- Error: clone on receiver
  ()
```

**Error:** `clone` is not defined for `**?T` (receivers).

### 7. BranchMissingDrop

**File:** `Invalid/AffineChannels/BranchMissingDrop/`

```fst
main =
  let (s, r) = newA in
  let b = True in
  if b
  then drop s
  else sendA 1 s  -- Error: s not dropped in False branch
```

**Error:** Sender not consumed in all control-flow branches.

### 8. ImplicitReuse

**File:** `Invalid/AffineChannels/ImplicitReuse/`

```fst
loop s =
  sendA 0 s;
  loop s  -- Error: recursive reuse without clone
```

**Error:** Recursive function reuses sender without cloning.

### 9. SenderBoundTwice

**File:** `Invalid/AffineChannels/SenderBoundTwice/`

```fst
main =
  let (s, r) = newA in
  let t = s in
  drop s;
  drop t  -- Error: aliasing leads to double drop
```

**Error:** Sender aliased and dropped twice through different names.

---

## Unit Tests

### AffineWellFormedness.test

Tests well-formedness rules for affine channel types:

- **Valid:** `**!Int`, `**?Int`, `**!(**!Int)`, `**!(!Int;End)`, `**?(Int, Bool)`
- **Invalid:** `clone : **?Int -> ...`, `un **!Int`, `un **?Int`

### AffineConsumption.test

Tests sender consumption rules:

- **Valid:** Consumption via `sendA`, `drop`, `clone`
- **Invalid:** Unused sender, use after drop, send without drop

### AffineCloneRules.test

Tests cloning semantics:

- **Valid:** Clone produces two independent senders, chain cloning
- **Invalid:** Using original after clone, cloning receivers

### AffineDropRules.test

Tests weakening (drop) rules:

- **Valid:** Drop consumes sender, drop in all branches
- **Invalid:** Drop on receiver, double drop, drop then use

### AffineBranching.test

Tests affine discipline in control flow:

- **Valid:** Both branches consume, case expressions, nested branching
- **Invalid:** One branch ignores sender, send without subsequent drop

### AffineRecursion.test

Tests recursive function typing:

- **Valid:** Base case drops, recursive case sends and recurses, mutual recursion
- **Invalid:** Recursive reuse without clone, base case escaping

### AffineHigherOrder.test

Tests higher-order functions with affine types:

- **Valid:** Functions that consume senders, callbacks with cloned senders
- **Invalid:** Applying function twice, closure reuse

### AffineLinearInteraction.test

Tests interaction between affine and linear session types:

- **Valid:** Coexistence of affine and linear channels
- **Invalid:** Type mismatches between affine and linear operations

### AffineReceiverLinearity.test

Tests receiver linearity:

- **Valid:** Single consumption via `receiveA`
- **Invalid:** Using receiver after consumption, cloning receivers

---

## Property-Based Tests

**File:** `test/property/AffineChannelsProperties.hs`

Haskell/QuickCheck properties for semantic verification of affine channel behavior.

### Abstract Model

```haskell
data Msg a = Payload a | Terminator

data Chan a = Chan
  { senderCount :: Int
  , queue       :: [Msg a]
  }
```

### Properties Tested

| # | Property | Description |
|---|----------|-------------|
| 1 | **Termination Soundness** | All senders dropped → Terminator received |
| 2 | **No Early Termination** | Terminator not emitted while senders exist |
| 3 | **Exactly One Terminator** | Exactly one `Nothing` per channel lifetime |
| 4 | **Message Preservation** | Every sent value is received |
| 5 | **No Duplication** | No value received more times than sent |
| 6 | **FIFO (Single Producer)** | Received order equals sent order |
| 7 | **Multi-Producer Multiset** | Multiset of received equals multiset of sent |
| 8 | **Sender Count Non-Negative** | `senderCount` never goes below 0 |
| 9 | **No Messages After Terminator** | Queue contains only Terminator after close |
| 10 | **Clone Preserves Count** | Cloning does not lose senders |

### Running Property Tests

```bash
stack test --test-arguments "--spec affine-channels-properties"
```

---

## Key Affine Channel Concepts Tested

### Affine vs Linear

- **Affine (`**!T`, `**?T`)**: Can be used **at most once** (weakening allowed)
- **Linear**: Must be used **exactly once** (no weakening)

### Sender Operations

| Operation | Type | Consumes? |
|-----------|------|-----------|
| `sendA` | `a -> **!a -> ()` | No (but must eventually drop) |
| `drop` | `**!a -> ()` | Yes |
| `clone` | `**!a -> (**!a, **!a)` | Yes (produces two new senders) |

### Receiver Operations

| Operation | Type | Consumes? |
|-----------|------|-----------|
| `receiveA` | `**?a -> Maybe (a, **?a)` | Yes (returns fresh receiver) |

### Termination Protocol

1. Each sender must be explicitly dropped
2. When sender count reaches 0, `Terminator` is appended
3. Receiver gets `Nothing` to signal channel closure
4. Exactly one `Nothing` per channel lifetime

---

## Files Created

### Valid Programs (8)
- `SingleProducerTermination.fst` + `.expected`
- `MultiProducerClone.fst` + `.expected`
- `DropOnlyTermination.fst` + `.expected`
- `RecursiveSend.fst` + `.expected`
- `InterleavingSumCheck.fst` + `.expected`
- `LargeCloneFan.fst` + `.expected`
- `RecursiveCloneProducers.fst` + `.expected`
- `ExactlyOneNothing.fst` + `.expected`

### Invalid Programs (9)
- `MissingDrop.fst`
- `DoubleDrop.fst`
- `UseAfterDrop.fst`
- `ImplicitDuplication.fst`
- `ReceiverDuplication.fst`
- `CloneReceiver.fst`
- `BranchMissingDrop.fst`
- `ImplicitReuse.fst`
- `SenderBoundTwice.fst`

### Unit Tests (9)
- `AffineWellFormedness.test`
- `AffineConsumption.test`
- `AffineCloneRules.test`
- `AffineDropRules.test`
- `AffineBranching.test`
- `AffineRecursion.test`
- `AffineHigherOrder.test`
- `AffineLinearInteraction.test`
- `AffineReceiverLinearity.test`

### Property Tests (1)
- `AffineChannelsProperties.hs`

---

## Total Test Count

| Category | Count |
|----------|-------|
| Valid Programs | 8 |
| Invalid Programs | 9 |
| Unit Test Files | 9 |
| Property Tests | 10 |
| **Total** | **36 tests** |

---

## Author

Rodrigo Neto (Neto-Testing)
