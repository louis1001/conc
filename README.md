# Conc

A concatenative programming language to work as an intermediate representation
for my Virual Machine ([VM in C](https://github.com/louis1001/bytecode_target)).

Inspired by Alexey Kutepov ([Tsoding](https://twitch.tv/tsoding))'s Porth: https://gitlab.com/tsoding/porth.
I'm doing this after a few months of watching the making of that language, so I'm trying really
hard not to just copy it directly.

That said, I'm already doing the stack-based typechecking, but that was also inspired by WASM's.

# Examples

A basic program needs a `main` function, and this can return either nothing or a u64 to pass as status code.

```
fn main || do
end
```

## Addition

```
fn main || do
    10 20 + debug # Will print 30
end
```

## Loops

```
fn main || do
    loop # Will loop infinitely
        "hello " puts
    end
end
```

## If statement
```
fn main || do
    10 # Pushes this value at the start

    1 2 < if
        2 + # Type checker accepts modifying the stack in a block,
            # as long as it doesn't affect its shape

        "1 is less than 2" # At the end of the if, there's a Ptr and U64 in the stack.
    else
        "1 is not less than 2" # Since it matches in both branches, it's valid
    end puts

    " - debug - " puts

    debug # And prints it at the end
    "\n" puts
end
```

## FizzBuzz
```
fn main || do
    1 while dup 200 < do
        dup 3 % 0 = if
            "Fizz" puts
        end

        dup 5 % 0 = if
            "Buzz" puts
        end

        dup 3 %
        over 5 %
        * 0 !=     # (x % 3) * (x % 5) != 0 -> x isn't multiple of 3 or 5
        if
            dup debug
        end

        '\n' putc
        
        1 + # increment
    end drop
end
```

## Factorial

In this example, you can see function declaration and function call. The order of the functions is not important.

```
fn main || do
    6
    "Factorial of " puts dup debug ": " puts
    !factorial

    debug
    '\n' putc
end

fn factorial |u64| -> |u64| do
#   n > 2: n * factorial(n-1)
    dup 2 > if
        dup 1 - # n-1
        !factorial *
    # else return n
    end
end
```

README will be updated as I go along.