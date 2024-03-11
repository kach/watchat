# WatChat

This is the codebase for our project on a cognitively-inspired automatic explanation engine for JavaScript. You can read more in our [preliminary report](https://arxiv.org/abs/2403.05334) presented at PLATEAU 2024:

**WatChat: Explaining perplexing programs by debugging mental models**

_Kartik Chandra, Tzu-Mao Li Rachit Nigam, Joshua Tenenbaum and Jonathan Ragan-Kelley_

> Often, a good explanation for a program's unexpected behavior is a bug in the programmer's code. But sometimes, an even _better_ explanation is a bug in the programmer's _mental model_ of the language they use. Instead of merely debugging our current code ("giving the programmer a fish"), what if our tools could directly debug our mental models ("teaching the programmer to fish")?
> 
> In this paper, we apply ideas from computational cognitive science to do exactly that. Given a perplexing program, we infer potential misconceptions the programmer might have, and analyze them to provide succinct, useful explanations of the program's observed behavior. Our methods can even be inverted to _synthesize_ pedagogical example programs for diagnosing and correcting misconceptions in students.

---

Contents:
- `js.rkt`, `js-test.rkt` - JS "misinterpreter" and tests
- `js-wat.rkt` - explanation solver
- `js-wedge.rkt`, `js-wedge-all.rkt` - concept inventory generator
- `watchat.rkt`, `index.html` - server for web front-end
