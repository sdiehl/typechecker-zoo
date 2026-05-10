# Type Systems Overview

So why do we build type systems? The answer is obviously because they are awesome and intellectually interesting. But second to that, because they are genuinely useful.

Let's start with some background; you can technically skip this section and just dive into the code if you fancy, but it is helpful to set up some soft context before we dive into implementation.

Any discussion of type systems starts with the lambda calculus, a formal system developed by Alonzo Church in the 1930s to express computation. It is the minimal, universal programming language. Its syntax consists of just three elements: variables, function abstractions (a way to define an anonymous function), and function application (calling a function). For instance, the identity function, which simply returns its input, is written as \\( \lambda x. x \\). Despite this simplicity, any computable problem can be expressed and solved within the lambda calculus.

Next, we introduce type systems. A type system is a set of rules that assigns a property, known as a **type**, to the constructs of a program, such as variables, expressions, and functions. The primary purpose is to reduce bugs by preventing operations that don't make sense, like dividing a number by a string. This process of verifying that a program obeys its language's type rules is called type checking.

## A System of Types

At the heart of any type system is a set of formal rules for making logical deductions about a program. These deductions are called **judgments**. A judgment is an assertion that a piece of code has a certain property. The most common kind of judgment you will encounter is a **typing judgment**, which asserts that a given expression has a particular type within a specific context. We write this formally using a "turnstile" symbol \\( \vdash \\).

The general form of a typing judgment looks like this:

\\[ \Gamma \vdash e : T \\]

This statement is read as, "In the context \\( \Gamma \\), the expression \\( e \\) has the type \\( T \\)." The context \\( \Gamma \\) is essentially a map from variable names to their types. For example, \\( x: \text{Int}, f: \text{Bool} \to \text{Int} \\) is a context where the variable \\( x \\) has type \\( \text{Int} \\) and \\( f \\) has a function type from \\( \text{Bool} \\) to \\( \text{Int} \\). As we enter deeper scopes in a program, like inside a function body, we extend the context with new variable bindings. This is often written as \\( \Gamma, x:T \\), which means "the context \\( \Gamma \\) extended with a new binding stating that variable \\( x \\) has type \\( T \\)."

Typing judgments are derived using **inference rules**. An inference rule states that if you can prove a set of judgments, called the premises, then you can conclude another judgment, called the conclusion. When working with formal type systems, inference rules follow a consistent structural pattern that makes them easier to read and understand. Every rule has the form:

\\[ \frac{\text{premises}}{\text{conclusion}} \text{(Rule-Name)} \\]

This notation should be read as: "If all the premises above the line are true, then the conclusion below the line is also true." The premises represent the conditions that must be satisfied, while the conclusion represents what we can deduce when those conditions hold. The rule name provides a convenient label for referencing the rule in discussions and proofs.

If a rule has no premises, it is an **axiom**, a self-evident truth that requires no prior proof to hold. For example, an axiom rule that establishes the type of the literal zero might look like:

\\[ \frac{}{\Gamma \vdash 0 : \text{Int}} \text{(T-Zero)} \\]

This rule has no premises above the line, making it an axiom. It simply states that in any context \\( \Gamma \\), the literal \\( 0 \\) has type \\( \text{Int} \\).

These axioms typically handle simple cases like variable lookups or literal values (sometimes called **ground types**). Rules with multiple premises, separated by spacing or explicit conjunction symbols, require all conditions to be satisfied simultaneously before the conclusion can be drawn.

A foundational rule in nearly every type system is the variable lookup rule, which lets us find the type of a variable from the context:

\\[ \frac{x:T \in \Gamma}{\Gamma \vdash x : T} \\]

This rule is not an axiom because it has one premise. It reads: "If the type binding \\( x:T \\) is present in the context \\( \Gamma \\), then we can conclude that in context \\( \Gamma \\), the expression \\( x \\) has type \\( T \\)." It formally defines the action of looking up a variable's type in the current environment.

By defining a collection of these inference rules, we create a complete type system. Each rule defines how to determine the type of a specific kind of expression, like a function call, a literal value, or an if-then-else block. For instance, a rule for function application would require as its premises that we first prove the function itself has a function type \\( T \to U \\) and that its argument has the corresponding input type \\( T \\). If we can prove those premises, the rule allows us to conclude that the entire function application expression has the output type \\( U \\). By repeatedly applying these rules, we can build a derivation tree that starts from axioms about variables and literals and culminates in a single judgment about the type of our entire program, thereby proving it is well-typed.

## Judgements

A common inference rule with multiple premises is the one for function application, which determines the type of a function call. In lambda calculus, this is simply an expression \\( e_1 \ e_2 \\), where \\( e_1 \\) is the function and \\( e_2 \\) is the argument. To assign a type to this expression, we must first determine the types of both the function and its argument. The rule, often called "application" or "elimination for functions" (\\( \to E \\)), is written as follows:

\\[ \frac{\Gamma \vdash e_1 : T_1 \to T_2 \quad \Gamma \vdash e_2 : T_1}{\Gamma \vdash e_1 \ e_2 : T_2} \\]

This rule has two premises above the line. It states that to conclude that the application \\( e_1 \ e_2 \\) has type \\( T_2 \\), we must first prove two things in the same context \\( \Gamma \\). First, we must show that \\( e_1 \\) has a function type, written as \\( T_1 \to T_2 \\), which means it takes an input of type \\( T_1 \\) and produces an output of type \\( T_2 \\). Second, we must show that the argument \\( e_2 \\) has the correct input type \\( T_1 \\). If both of these premises hold, the rule allows us to deduce that the entire expression \\( e_1 \ e_2 \\) results in the function's output type, \\( T_2 \\).

Now, let's look at an example that chains three judgments together to type check the expression \\( f \ x \\). We'll work within a context \\( \Gamma \\) that contains bindings for both \\( f \\) and \\( x \\), specifically \\( \Gamma = f:\text{Int} \to \text{Bool}, x:\text{Int} \\). Our goal is to prove that \\( \Gamma \vdash f \ x : \text{Bool} \\).

Our derivation begins with two simple variable lookups, which are our axioms. These will form the premises of our application rule:

1. **First Judgment (Variable Lookup for \\( f \\))**: We use the variable rule to find the type of \\( f \\). Since \\( f:\text{Int} \to \text{Bool} \\) is in our context \\( \Gamma \\), we can conclude:
   \\[ \frac{f:\text{Int} \to \text{Bool} \in \Gamma}{\Gamma \vdash f : \text{Int} \to \text{Bool}} \\]

2. **Second Judgment (Variable Lookup for \\( x \\))**: Similarly, we look up the type of \\( x \\). The binding \\( x:\text{Int} \\) is in \\( \Gamma \\), so we can conclude:
   \\[ \frac{x:\text{Int} \in \Gamma}{\Gamma \vdash x : \text{Int}} \\]

3. **Third Judgment (Function Application)**: Now we have the necessary premises to use the function application rule. We substitute \\( e_1 \\) with \\( f \\), \\( e_2 \\) with \\( x \\), \\( T_1 \\) with \\( \text{Int} \\), and \\( T_2 \\) with \\( \text{Bool} \\). Since our first two judgments successfully proved the premises, we can now form the final conclusion:
   \\[ \frac{\Gamma \vdash f : \text{Int} \to \text{Bool} \quad \Gamma \vdash x : \text{Int}}{\Gamma \vdash f \ x : \text{Bool}} \\]

Putting all of this together, we can represent the full derivation as a tree of nested inference rules, showing how the final judgment is built from the axioms for variable lookup:

\\[
\frac{
\frac{f:\text{Int} \to \text{Bool} \in \Gamma}{\Gamma \vdash f : \text{Int} \to \text{Bool}}
\quad
\frac{x:\text{Int} \in \Gamma}{\Gamma \vdash x : \text{Int}}
}{
\Gamma \vdash f \ x : \text{Bool}
}
\\]

This nested structure is called a **derivation tree** or **inference tree**. Each node in the tree corresponds to an application of an inference rule, and the leaves are axioms (variable lookups from the context). The tree visually demonstrates how the type checker starts from basic facts (the types of variables in the context) and applies rules step by step to reach a conclusion about the type of a complex expression. In this example, the root of the tree is the judgment \\( \Gamma \vdash f \ x : \text{Bool} \\), and its two children are the judgments for \\( f \\) and \\( x \\), each justified by their respective axioms. This process generalizes to larger programs, where the derivation tree grows to reflect the structure of the program and the logical flow of type information.

## Terms and Types

Historically, many programming languages enforced a strict separation between different layers of abstraction, a concept known as stratification. In this model, you had a "term language" and a "type language" which were syntactically and conceptually distinct. The term language consists of the expressions that actually compute values and run at runtime, like \\( 5 + 2 \\) or \\( \text{if } x \text{ then } y \text{ else } z \\). The type language, on the other hand, consists of expressions that describe the terms, like \\( \text{Int} \\) or \\( \text{Bool} \to \text{Bool} \\). These two worlds were kept separate; a type could not appear where a term was expected, and vice versa.

This stratification could be extended further. To bring order to the type language itself, a third layer called the "kind language" was often introduced. A kind can be thought of as the "type of a type." For example, a concrete type like \\( \text{Int} \\) has the simplest kind, \\( * \\) (often pronounced "type"). But a type constructor like \\( \text{List} \\) isn't a type on its own; it's something that takes a type and produces a new one. \\( \text{List} \\) takes \\( \text{Int} \\) to produce \\( \text{List} \ \text{Int} \\). Therefore, its kind is \\( * \to * \\). This creates a rigid hierarchy: terms are classified by types, and types are classified by kinds. This clear separation makes type checking simpler and more predictable.

However, a major trend in the design of modern, powerful type systems has been to move away from this strict stratification and instead unify the term and type languages into a single, consistent syntactic framework. In these unified systems, the line between what is a "value" (a term) and what is a "description" (a type) begins to blur. The language's grammar allows expressions that can be interpreted at different "levels" or "universes." For instance, you might have a universe \\( \text{Type}_0 \\) which contains simple types like \\( \text{Bool} \\). Then you would have a higher universe, \\( \text{Type}_1 \\), whose only member is \\( \text{Type}_0 \\). This allows you to write functions that operate on types themselves, a key feature of dependently typed languages. More on this later.

To make this concrete, let's consider a simple, stratified language for arithmetic. We can define its term language (\\( e \\)) and its corresponding type language (\\( \tau \\)) separately. The terms are the expressions we can compute, and the types are the static labels we can assign to them. Their definitions might look like this:

\\[
\begin{align*}
\text{terms} \quad e &::= n \mid e\_1 + e\_2 \mid \text{iszero}(e) \mid \text{true} \mid \text{false} \mid \text{if } e\_1 \text{ then } e\_2 \text{ else } e_3 \\\\
\text{types} \quad \tau &::= \text{Nat} \ \mid \ \text{Bool}
\end{align*}
\\]

Here, the term language \\( e \\) defines natural numbers (\\( n \\)), addition, a function to check for zero, boolean constants, and a conditional. The type language \\( \tau \\) is much simpler; it only contains the types \\( \text{Nat} \\) and \\( \text{Bool} \\). In this stratified system, an expression like \\( \text{Nat} + 5 \\) would be a syntax error because \\( \text{Nat} \\) belongs to the type language and cannot be used in a term-level operation like addition. In a more modern, unified system, this rigid distinction would be relaxed.

## Type Checking and Type Reconstruction

While the process of verifying that a program adheres to its type rules is called type checking, a related and historically significant challenge is **type reconstruction**, more commonly known as **type inference**. The goal of type inference is to have the compiler automatically deduce the types of expressions without the programmer needing to write explicit type annotations. For many years, developing algorithms that could perform full type inference for increasingly expressive languages was an active and vital area of research. The promise was seductive: achieve all the safety guarantees of a static type system without the verbose, manual effort of annotating every variable and function.

In more recent decades, the focus on achieving complete type inference has diminished. The primary reason is that as type systems have grown more powerful and complex, full inference has become computationally intractable or, in many cases, fundamentally undecidable. Modern languages often include features like higher-rank polymorphism (passing polymorphic functions as arguments), GADTs, and various forms of type-level programming where types themselves can involve computation. For these systems, a general algorithm that can always infer the single "best" type for any given expression simply does not exist. Attempting to do so would lead to impossibly complex algorithms and can result in inferred types that are enormous and incomprehensible to the programmer.

As a result, many modern statically-typed languages have converged on a practical and elegant middle ground: **bidirectional type checking**. Instead of having a single mode that always tries to infer types, a bidirectional checker operates in two distinct modes: a "checking" mode and a "synthesis" mode.

1. **Checking Mode**: In this mode, the algorithm verifies that an expression \\( e \\) conforms to a known, expected type \\( \tau \\). Information flows "down" from the context into the expression. We ask the question: "Can we prove that \\( e \\) has type \\( \tau \\)?"
2. **Synthesis Mode**: In this mode, the algorithm computes, or "synthesizes," a type for an expression \\( e \\) without any prior expectation. Information flows "up" from the expression's components. Here, we ask: "What is the type of \\( e \\)?"

This duality provides a powerful framework. The language designer can specify which syntactic constructs require annotations and which do not. For example, the arguments of a top-level function might require explicit annotations (putting the checker in checking mode for the function body), but the types of local variables within that body can be inferred (synthesized). This approach neatly sidesteps the difficulties of full inference by requiring the programmer to provide annotations only at key boundaries where ambiguity would otherwise arise. It offers a "best of both worlds" scenario: the convenience of local inference with the clarity and power of explicit annotations for complex, polymorphic, or ambiguous parts of the code, representing a theoretical sweet spot that balances expressiveness, usability, and implementability. More on this later.

## The Frontiers

Since the late 1970s, researchers have noticed striking structural parallels between computation, formal logic, and category theory. This observation, sometimes called ["computational trinitarianism"](https://ncatlab.org/nlab/show/computational+trilogy), suggests that these three disciplines are studying the same underlying mathematical structures from different angles. The connections are genuine and mathematically rigorous, though their practical implications for everyday programming remain a matter of ongoing work rather than settled fact.

The central insight here is the **Curry-Howard correspondence**, which establishes a formal duality between type systems and logical proof systems. Under this correspondence, a type can be read as a logical proposition, and a well-typed program constitutes a constructive proof of that proposition. This is not mere analogy: the same formal structures appear in both domains, and results proven in one setting transfer directly to the other.

The correspondence extends naturally in several directions:

- A **Proposition** in logic corresponds to a **Type** in programming, which corresponds to an **Object** in category theory.
- A **Proof** corresponds to a **Program** (or term) of that type, which corresponds to a **Morphism** between objects.
- **Proof simplification** corresponds to **Program evaluation**, which corresponds to composition of morphisms.

What does this buy us in practice? Consider a function to retrieve the first element of a list. A naive signature like `head(List<T>) -> T` makes a promise the implementation cannot keep: it claims to produce a value of type `T` for any list, but an empty list has no first element. The type is a false proposition.

A more honest signature is `safeHead(List<T>) -> Maybe<T>`. The `Maybe` type forces the caller to handle the possibility of absence. This is a modest example, but it illustrates the key point: the type system enforces a contract, and the compiler checks that contract statically. The proposition expressed by the type is actually true of any program that typechecks.

Dependent type systems push this further. In a language like Agda, Coq, or Lean, you can define a type `SortedList<T>` whose values are lists together with proofs that they are sorted. A merge function with signature `merge(SortedList<T>, SortedList<T>) -> SortedList<T>` must construct not just the merged list but also a proof that the result is sorted. The compiler will reject any implementation that fails to provide this proof.

This is genuinely powerful, but it comes with costs that are easy to understate. Writing proofs requires expertise that most programmers do not have. The proof burden can exceed the implementation effort by an order of magnitude. Proof assistants have made significant progress on automation, but we are far from a world where deep correctness properties fall out automatically. The languages that support these features remain niche, used primarily in formal verification of critical systems and in mathematical research.

There is a tempting vision here: if types are propositions and programs are proofs, perhaps we could specify what we want as a type and have the computer synthesize the program automatically. This is the dream of program synthesis from specifications. The reality is more modest. Current synthesis tools work well for small, highly constrained problems. Generating correct implementations from rich specifications remains an open research problem, with practical tools handling only limited domains.

As machine learning systems increasingly generate code, type systems offer one path toward trust. A type checker is a verifier: it accepts only programs that satisfy its rules, regardless of whether a human or a model wrote the code. Richer type systems raise the bar on what counts as acceptable, catching more errors before deployment. This is a reasonable engineering argument for investing in expressive types, though it is worth noting that most properties we care about in real systems, things like performance, security against side channels, or correct handling of distributed state, remain outside what current type systems can express.

The theoretical connections between logic, computation, and category theory are beautiful and have guided the design of programming languages for decades. They suggest that there is something deep and unified in these structures. But translating this theory into practical tools that working programmers can use remains ongoing work. The gap between what is possible in principle and what is practical in the field is real, and closing it will require continued research into proof automation, language design, and tooling.

These ideas are worth studying not because they will immediately transform how you write software, but because they reveal the mathematical bones beneath the code. Understanding why types work the way they do, and what they are capable of expressing, makes you a better designer of programs and languages alike. That understanding is its own reward.
