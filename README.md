# MiniCalc64

This is a [hexidecimal](https://en.wikipedia.org/wiki/Hexadecimal) postfix calculator written as an entry to [MinicubeJam04](https://itch.io/jam/minicubejam04).

# What is Postfix?

Postfix is also known as Reverse Polish Notation (RPN), and is a different approach to entering calculations than most people are used to (which is called "infix"). [This site](https://www.mathblog.dk/reverse-polish-notation/) likely does a much better job explaining things than I would.

In this calculator you type 16bit values, push them onto the stack, and then use operations (add, subtract, etc) to manipulate the values on the stack. Results that exceed 16bits will be truncated. Negative values are handled using [two's complement](https://en.wikipedia.org/wiki/Two%27s_complement).

To perform 1+2 (or in postfix: 1 2 +), you would type "1", push it to the stack (^ or START), type "2", push (^ or START), then add them together (+). This will result on a value of "3" on the stack.

This calculator was heavily influenced by [cccc](https://wiki.xxiivv.com/site/cccc.html), a calculator for the [varvara computer](https://wiki.xxiivv.com/site/varvara.html) written in [uxntal](https://wiki.xxiivv.com/site/uxntal.html).

# Basic Controls

![minicube](https://user-images.githubusercontent.com/10489588/188784299-e7062596-7eee-431d-a657-2b007ac59007.gif)

- Arrows/Dpad: Move cursor
- A: Press highlighted button
- B: Delete last digit entered, or pop last item on stack if no digits remain
- Start: Push entry to stack
- C+Start: Clear the stack

# Buttons

Starting from the bottom left, moving left-to-right and top-to-bottom, the buttons on this calculator are:

- 0-F: The 16 hexidecimal digits

- Push: Push the current entry to the stack
- Pop: Pop the last entry off the stack
- Swap: Swap the last two entries on the stack
- Duplicate: Duplicate the last entry on the stack

- Add: Add last two entries on the stack
- Subtract: Subtract last stack entry from second last entry
- Multiply: Multiply last two stack entries
- Divide: Divide second last stack entry by the last stack entry

- AND: Logical AND last two stack entries
- OR: Logical OR last two stack entries
- Shift Left: Shift last stack entry left one bit
- Shift Right: Shift last stack entry right one bit

# Shortcuts (instead of using the specific buttons)

- C+A: Swap
- C+B: Duplicate

- C+Up: Add
- C+Down: Subtract
- C+Left: Multiply
- C+Right: Divide

- Start+Up: Logical AND
- Start+Down: Logical OR
- Start+Left: Shift left
- Start+Right: Shift right

