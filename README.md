This project is now under the free ammonia foundation.  

Shard is still at a VERY EARLY STAGE, like not even usable yet  
If you like this concept then PLEASE help out  
I cant do it all by myself.. :/  

# Contributing Checklist
- Join our [Discord](https://discord.gg/z3Qnr87e7c)
- DO NOT RUSTFMT

# TODOS
- [x] rework parser to support new typing behaviour
	- [ ] rework analysis to follow
	- [ ] (prob not necessary) rework codegen to follow
- [ ] make a step in between the ast and mir (hir) which is basically a typed ast
	- [x] laid the groundwork
	- [ ] no-op version with little to no type inference
	- [ ] type inference to the moon
	- [ ] generics into template generation?? (presumably this is checked here, mir just applies templates)
- [ ] get the website back up 
	- [x] move shard to faf (also make a faf org)??
	- [ ] update [sherbert](https://github.com/shard-org/sherbert) with new docs
	- [ ] sherbert fix syntax highlighting (prob get docs first)
- [ ] diff syntax for `<` since that conflicts with type generics `Foo<Bar>`
- [ ] define move semantics. what's copy by default?
- [x] bully @interacsion into either making a borrow checker or shutting up about it :) (preferably option b)
- [ ] integrate core traits as operator overloads
- [ ] draft up shard docs:
	- [ ] basics + typing
	- [ ] core lib
- [ ] update nightly version to one with good features/stability
- [ ] fix the bump allocator
	- [ ] miri error (rust bs semantics are driving me crazy)
	- [x] possible double free on panic? (needs investigation)
