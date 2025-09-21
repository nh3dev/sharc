This project is now under the free ammonia foundation :0  

Shard is still at a VERY EARLY STAGE, like not even usable yet  
If you like this concept then PLEASE help out  
I cant do it all by myself.. :/  

# Contributing Checklist
- Join our [Discord](https://discord.gg/z3Qnr87e7c)
- DO NOT RUSTFMT

# Examples

```rs
let x = 5;
let addx = |y| x + y;
add(10)
```

```rs
let printf_int = extern "printf" |fmt: raw &[u8], i: i32|: i32;
printf_int("hello %d\n\0", 5 + 10);
```

```rs
let average = |nums: &[u32]|: u32 {
	let sum = loop let (sum, i) = (0, 0) {
		if i == a.len { break sum; }
		(sum + a[i], i + 1)
	};
	sum / a.len
};

average(&[1, 2, 3, 4, 5]) // => 3
```

# TODOS
- [x] rework parser to support new typing behaviour
	- [x] rework analysis to follow
- [ ] make a step in between the ast and mir (hir) which is basically a typed ast
	- [x] laid the groundwork
	- [x] no-op version with little to no type inference
	- [ ] type inference to the moon
	- [ ] generics into template generation?? (presumably this is checked here, mir just applies templates)
- [ ] get the website back up 
	- [x] move shard to faf (also make a faf org)??
	- [ ] update [sherbert](https://github.com/shard-org/sherbert) with new docs
	- [ ] sherbert fix syntax highlighting (prob get docs first)
- [ ] diff syntax for `<` since that conflicts with type generics `Foo<Bar>`
- [ ] define move semantics. what's copy by default?
- [x] bully @interacsion into either making a borrow checker or shutting up about it :) (preferably option b)
- [ ] do actual trait resolution
- [ ] integrate core traits as operator overloads
- [ ] draft up shard docs:
	- [ ] basics + typing
	- [ ] core lib
- [ ] update nightly version to one with good features/stability
- [ ] fix the bump allocator
	- [ ] miri error (rust bs semantics are driving me crazy)
	- [x] possible double free on panic? (needs investigation)
