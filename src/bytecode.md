
Hebi4's bytecode is fixed width at 4 bytes per instruction. 1 byte is used for the tag, and the remainder
is used for instruction operands.

Instruction operands may be encoded using one of the following schemes:
- tag + 2×8b operands
- tag + 3×8b operands
- tag + 1×8b operand, 1×16b operand
- tag + 1×24b operand

Operands are either 8-bit stack slots ("registers"), 16-bit constant slots, or 16-bit immediate values.

