
# Prototype `add_colors()`

Purpose:

* Given a set of colors previously assigned
* Request to add N colors to the set
* Return new set of categorical colors

## Input arguments

* `given_colors` - character vector of colors; bonus points: list
of character vectors or color functions - convert color function
to vector by using its color steps, same as `jamba::showColors()`
* `n` - number of new colors to add
* `color_fn` - function to generate new colors for testing.
Default `rainbowJam()` or in future `fibonacci_colors()`

## Workflow

1. Create `new_colors` with `color_fn`
2. Call `slot_colors()` to assign `given_colors` to `new_colors`
3. If `n` colors are unassigned, assign them in order.
4. If fewer than `n` colors remain, repeat step (1) with `n+1` colors.
