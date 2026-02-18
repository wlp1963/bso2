# BSO2 Zero Page Reference

Monitor ZP range: `$30-$8F`  
User ZP range: `$90-$FF`

| Label | Size | Address | Use | Ref |
|---|---:|---:|---|---:|
| CORE_WORKSPACE | 72 bytes | `$30-$77` | Monitor workspace (pointers, parser, debug scratch) | 1 |
| GAME_ASK_PENDING | 1 byte | `$78` | One-shot game prompt latch | 3 |
| BRK_FLAG | 1 byte | `$79` | Debug/BRK context-valid flag | 5 |
| TERM_COLS | 1 byte | `$7A` | Terminal width preference (`28/50/84`) | 7 |
| RESERVED_GAP_A | 5 bytes | `$7B-$7F` | Reserved gap before trampolines | |
| RST_HOOK | 3 bytes | `$80-$82` | Reset trampoline target | 9 |
| NMI_HOOK | 3 bytes | `$83-$85` | NMI trampoline target | 11 |
| IRQ_HOOK | 3 bytes | `$86-$88` | IRQ trampoline target | 13 |
| RESERVED_GAP_B | 7 bytes | `$89-$8F` | Reserved gap after trampolines | |
| USER_ZP | 112 bytes | `$90-$FF` | User-owned ZP range | 15 |

## Command Map (BSO2)

| Cmd | Symbol(s) | Purpose refs |
|---|---|---|
| `H`, `H P` | Fixed-address help lines | 3, 7, 9, 11, 13, 15 |
| `V` | Vector chain display | 9, 11, 13 |
| `!M` | Manual byte poke for pinned bytes | 3, 7 |
| `Q` | Halt/resume flow tied to NMI/Reset | 9, 11 |
| `R`, `N`, `X` | Debug/step/execute flow using context + vectors | 5, 9, 11, 13 |

## Reference Legend

1. Core monitor ZP workspace block (`$30-$77`)  
3. Game prompt latch byte (`$78`)  
5. Debug/BRK context flag (`$79`)  
7. Terminal-width pin byte (`$7A`)  
9. Reset trampoline (`$80-$82`)  
11. NMI trampoline (`$83-$85`)  
13. IRQ trampoline (`$86-$88`)  
15. User-reserved ZP (`$90-$FF`)
