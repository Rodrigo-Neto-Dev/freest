# FreeST Affine Channels — Test Suite

> This doc supersedes the affine-channels section of
> `freest/test/Neto-Testing.md`, which is largely out of date with the
> post-migration architecture. For the pre-migration catalogue of
> `Valid/AffineChannels` programs and their original expected outputs
> (predating this document), see `freest/test/Neto-Testing.md`.
> All post-migration run commands, files actually driven, and
> coverage gaps are documented below.

The FreeST compiler ships with three test suites exercised under
`stack test`. This document describes the **affine-channels-specific**
portion of the suite: how to run it, what each piece covers, and where
to look when something fails.

## Quick commands

> All commands are run from the `freest/` subdirectory.
>
> **How `--match` works in hspec:** it is a **literal substring
> match** against the test path (module → describe → it). It is NOT
> a regex DSL — `|`, `/.../`, anchors, etc. do not behave as anchors
> or alternation. Patterns that look like regex silently match
> nothing if those literal characters never appear in the test path.
> Recommended strategy: **match against a substring of the
> `.test` filename** (e.g. `AffineChannelKinds`) — these strings are
> guaranteed to appear in test paths.
>
> **Shell quoting:** the examples below use single-quoted Bash-style
> outer strings. On PowerShell, single-quoted outer strings work
> unchanged (no expansion). On `cmd.exe`, the outer single quotes are
> not stripped — substitute double quotes.

From the `freest/` subdirectory:

```sh
# All AffineChannels program tests (and only those) — 26 prog tests.
stack test :prog --ta '-m AffineChannels'

# All 3 Affine unit specs (Valid + Invalid + Equivalence) — 22 entries.
stack test :unit --ta '--match AffineChannel'

# Just the Valid spec — 8 entries (the AffineChannelKinds.test file).
stack test :unit --ta '--match AffineChannelKinds'

# Just the Invalid spec — 7 entries (the AffineChannelsIllFormedTypes.test file).
stack test :unit --ta '--match AffineChannelsIllFormedTypes'

# Just the Equivalence spec — 7 entries (the AffineChannelTypeEquivalence.test file).
stack test :unit --ta '--match AffineChannelTypeEquivalence'

# Everything (library + prog + unit).
stack test
```

> **Debugging a single failing entry:** the failing test's full path is
> already printed by hspec. The safe path is to copy whatever hspec
> actually prints and pass that verbatim to `--match`. Substring
> shapes that omit trailing path components (the `line:col–line:col`
> end-range, for instance) can silently produce 0 examples because
> `--match` is a literal substring against the full IT label — so a
> substring that stops short of where the label ends has nothing to
> anchor to. On PowerShell ensure the console-host encoding is
> UTF-8 (e.g. `chcp 65001` on cmd.exe or
> `[Console]::OutputEncoding = [System.Text.Encoding]::UTF8`, which
> works in Windows PowerShell 5.x and PowerShell 7+); otherwise the
> `–` end-range character can be mangled into a hyphen and the match
> will silently miss.

## Layout

```
freest/test/
├── prog/
│   ├── Valid/AffineChannels/    (15 programs + .expected)
│   └── Invalid/AffineChannels/  (11 programs)
└── unit/
    ├── AffineChannelKinds.test          (valid kind assertions)
    ├── AffineChannelsIllFormedTypes.test (ill-formed, must reject)
    ├── AffineChannelTypeEquivalence.test (reflexivity only)
    ├── AffineKindingValidSpec.hs        (driver, errorsAreFailures)
    ├── AffineKindingInvalidSpec.hs      (driver, errorsAreSuccesses)
    └── AffineEquivalenceSpec.hs         (driver, equivalent shouldBe True)
```

Notes:
* `AffineWellFormedness.test` was removed during post-migration
  cleanup because its annotations (`: *T`, `: 1T`, etc.) are
  permissive under FreeST's subkinding (see header of `Neto-Testing`
  migration commentary/history). Documented in the file itself for
  posterity.
* Eight other `Affine*.test` files in `freest/test/unit/` (`Branching`,
  `CloneRules`, `Consumption`, `DropRules`, `HigherOrder`,
  `LinearInteraction`, `ReceiverLinearity`, `Recursion`) were deleted
  during the post-migration cleanup (see `freest/AFFINE_MIGRATION_REPORT.md`
  §9 follow-ups). They used a `{vars} |- term : type` sequent-style
  format that the existing `UnitSpecUtils` helpers don't support, and
  their entries referenced the pre-migration bytecode API
  (`clone` rather than `cloneAS`, etc.). They were also largely
  commented-out bodies, so they wouldn't type-check under the
  post-migration architecture even if a helper existed. The linearity
  invariants they documented are preserved in the migration report
  §9 deletion entry for future re-implementation if a `mkSequentSpec`
  helper becomes available.

### Note on `AffineWellFormedness.test` (deleted)

The original `AffineWellFormedness.test` asserted `: *T`, `: 1T`,
`: *C`, `: *S` annotations on affine wrappers; under
`Multiplicity`'s first subsort clause `_ <: Lin{} = True` (making
`Un <: Lin` valid) and `Prekind`'s `Channel <: Top = True`, all such
permutations of wrapper-kind annotations are accepted. Hence the
file's assertions never rejected; the orphaned entries were deleted
rather than left as silent-pass dead data. (Reasoning recoverable
from git history; file removed in cleanup commit.)

## Program tests

* 15 Valid programs in `freest/test/prog/Valid/AffineChannels/`. Each
  has a `.fst` source and a `.expected` golden-output file. Run via
  `stack test :prog --ta '-m AffineChannels'`.
* 11 Invalid programs under `freest/test/prog/Invalid/AffineChannels/`.
  Each must be rejected at the typechecking stage.

## Unit tests

Three drivers deliver the affine unit-test entries:

| Spec | `.test` files driven | Mode |
|---|---|---|
| `AffineKindingValidSpec` | `AffineChannelKinds.test` | Errors are failures |
| `AffineKindingInvalidSpec` | `AffineChannelsIllFormedTypes.test` | Errors are successes |
| `AffineEquivalenceSpec` | `AffineChannelTypeEquivalence.test` | Reflexivity (via `t == u`) |

### AffineChannelKinds.test — 8 valid kind checks

Each entry is `case T : 1C` where `T` is a wrapper `**!(...)` or
`**?(...)` whose inner is a session-kind (`1S`) protocol. The
post-migration `Kinding` pass wraps the inner with `AffineSender` /
`AffineReceiver` constructors of kind `lc = 1C`.

### AffineChannelsIllFormedTypes.test — 7 ill-formed types

Each entry omits the kind annotation. The inner protocol is *not* a
session — proper unrestricted types (`Int`, `Float`), tuples, function
types, and compositions with non-session operands. The kind check must
reject.

Cases involving wrappers over `*C` (unrestricted shared channels, like
`*!Int`) are NOT listed here: FreeST's subkinding accepts `*C <: 1S`
because `Multiplicity`'s first subsort clause is `_ <: Lin{} = True`
(making `Un <: Lin` valid), and `Channel <: Session` is true from
`Prekind`'s subsort. Documented in the file's header comment.

### AffineChannelTypeEquivalence.test — 7 reflexivity assertions

The W-grammar treats `AffineSender` / `AffineReceiver` as opaque
constructors whose inner is reduced via the standard bisimulation. The
wrapper itself does NOT collapse with its dual in the grammar: e.g.
`Dual **?(s)` and `**!(Dual s)` are NOT bisimilar. So only reflexivity
tests are present. Each entry compares a wrapper with itself,
satisfied by the structural Eq instance on `Type` directly (`t == u`).

## What's NOT covered

* Dual identity at the affine wrapper layer (`Dual (**?(S))` ≢
  `**!(Dual S)` under W-grammar). Adding such a test would require
  either an explicit grammar rule for wrapper dual collapse or
  re-structuring the entry so the dual is reduced structurally before
  the wrapper.
* Affine receivers under the `*?S` shared-channel syntax (no such
  post-migration constructor; only `**?`).
* Affine branch trees with `&` / `+` over receivers (currently only one
  sender-side choice test is in `AffineChannelKinds.test`).

## Verifying changes locally

```sh
cd freest
stack test :prog --ta '-m AffineChannels'                          # 26 prog tests (15 Valid + 11 Invalid)
stack test :unit --ta '--match AffineChannel'                     # 22 unit entries (8 + 7 + 7)
stack test :unit --ta '--match AffineChannelKinds'                 # 8 entries (Valid spec only)
stack test :unit --ta '--match AffineChannelsIllFormedTypes'       # 7 entries (Invalid spec only)
stack test :unit --ta '--match AffineChannelTypeEquivalence'       # 7 entries (Equivalence spec only)
```

Pre-existing failures unrelated to this work:
* `BisimulationValid` / `EquivalenceValid.test:741` and `:757`
  (`IFRepeat` and `FE` examples in the wider unit suite) — not part of
  the affine migration.

## Author / history

* Originating migration: capability/session split in
  `freest/src/Syntax/Type/Internal.hs` and downstream passes. See
  `freest/AFFINE_MIGRATION_REPORT.md` for background.
* Test-suite additions layout authored to mirror
  `KindCheckValidSpec.hs` / `KindInvalidSpec.hs` /
  `BisimulationValidSpec.hs`. Driver discovery is automatic via
  `hspec-discover` (declared in `freest/test/unit/UnitSpec.hs`).
