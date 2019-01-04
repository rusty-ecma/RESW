# RESW
An experimental crate for writing JS from an AST provided by the [RESSA](https://github.com/freemasen/ressa) crate.

Since RESSA's primary purpose is to enable developers to write their JS dev tools in Rust, this is an important part of that eco system. The experimental status of this project is due to the current instability in both RESS and RESSA, when pinned to the versions listed in this crate's `Cargo.toml` the behavior should be sound.

## Basic Usage

Here is a trivial example of how you might use this crate.

```js
// example.js
function thing() {
    return 'stuff'
}

```

Using the above example, we can create a program that will just parrot out the input.

```rust
use resw::Writer;
use ressa::Parser;
use srd::fs::{read_to_string, File);

fn main() {
    let js = read_to_string("example.js").expect("failed to read example.js");
    let p = Parser::new(&js).expect("failed to create parser);
    let f = Filem:create("example.out.js");
    let mut w = Writer::new(f);
    for part in p {
        w.write_part(part).expect(&format!("failed to write part {:?}", part));
    }
}
```

If we were to run the above, assuming that `example.js` exists, it would write the following in `example.out.js`.

```js
// example.js
function thing() {
    return 'stuff';
}

```

not super exciting but I did say the example would be trivial. Check out the examples directory for a few more, slightly less trivial examples.

## Notes
### Running the Examples
`cargo run --example snippet_writer` will expect a _script_ unless you pass an argument "module" like this `cargo run --example snippet_writer -- module`

### That's Like Your Opinion... Man
Currently I have made some personal style choices for the generated output. My ultimate vision for this crate is to allow for configuring a large number of these decisions but until RESSA has a chance to add some needed features they will not be configurable. Here are a few things I can think of right now.

- comments are never included in the output
- empty function bodies are two braces on the same line seperated by a single space
- empty if, try, and loop bodies are two braces seperated by an empty new line
- new lines are always `\n`
- function or function identifiers are writted with no space before `(`
- function parameter lists are not surrounded with empty space
- arry and object literals have trailing commas
- object literals are always one property per line

### Building on unstable foundation
I have some ambitous things in store for RESS/RESSA for the next year, changes there will ultimatly break what this crate can do but with time, those changes will greatly enhance this crates features. Some notible things in the works:

#### RESS
- a new allocation free version of the `Scanner` has been implemented but needs testing
- I would like to add Typescript support

#### RESSA 
- Work has started to include the location of all node items
  - this will allow RESW to increase the fedility of the generated JS

## Contributing
If you are interested in helping, it woud be appreciayed. At this point I would encourage you to look at improving RESS or RESSA before trying to find a way to contribut here. If you have a contribution I ask that you please open an issue before digging too deep into it so we can both be on the same page. 

### Some places that would be great to see some help
- examples
  - My imagination is only so big, even just ideas for dev tools that might need a writer (that aren't a bable clone or js minifier) would be amazing
  - If you wanted to build a JS minifier I would be truely flattered
    - Though currently there is no mechanizism not include new line characters

