---
name: self-documenting-code
description: A guide for writing self-documenting code
---

Code should be self documenting

How you split logic into functions and shape the data they pass around determines how well a codebase holds up over time.

Semantic Functions

Semantic functions are the building blocks of any codebase, a good semantic function should be as minimal as possible in order to prioritize correctness in it. A semantic function should take in all required inputs to complete its goal and return all necessary outputs directly. Semantic functions can wrap other semantic functions to describe desired flows and usage; as the building blocks of the codebase, if there are complex flows used everywhere that are well defined, use a semantic function to codify them.

Side effects are generally undesirable in semantic functions unless they are the explicit goal because semantic functions should be safe to re-use without understanding their internals for what they say they do. If logic is complicated and it's not clear what it does in a large flow, a good pattern is to break that flow up into a series of self describing semantic functions that take in what they need, return the data necessary for the next step, and don't do anything else. Examples of good semantic functions range from quadratic_formula() to retry_with_exponential_backoff_and_run_y_in_between<Y: func, X: Func>(x: X, y: Y). Even if these functions are never used again, future humans and agents going over the code will appreciate the indexing of information.

Semantic functions should not need any comments around them, the code itself should be a self describing definition of what it does. Semantic functions should ideally be extremely unit testable because a good semantic function is a well defined one.

Pragmatic Functions

Pragmatic functions should be used as wrappers around a series of semantic functions and unique logic. They are the complex processes of your codebase. When making production systems it's natural for the logic to get messy, pragmatic functions are the organization for these. These should generally not be used in more than a few places, if they are, consider breaking down the explicit logic and moving it into semantic functions. For example provision_new_workspace_for_github_repo(repo, user) or handle_user_signup_webhook(). Testing pragmatic functions falls into the realm of integration testing, and is often done within the context of testing whole app functionality. Pragmatic functions are expected to change completely over time, from their insides to what they do. To help with that, it's good to have doc comments above them. Avoid restating the function name or obvious traits about it, instead note unexpected things like "fails early on balance less than 10", or combatting other misconceptions coming from the function name. As a reader of doc comments take them with a grain of salt, coders working inside the function may have forgotten to update them, and it's good to fact check them when you think they might be incorrect.

Models

The shape of your data should make wrong states impossible. If a model allows a combination of fields that should never exist together in practice, the model isn't doing its job. Every optional field is a question the rest of the codebase has to answer every time it touches that data, and every loosely typed field is an invitation for callers to pass something that looks right but isn't. When models enforce correctness, bugs surface at the point of construction rather than deep inside some unrelated flow where the assumptions finally collapse. A model's name should be precise enough that you can look at any field and know whether it belongs — if the name doesn't tell you, the model is trying to be too many things. When two concepts are often needed together but are independent, compose them rather than merging them — e.g. UserAndWorkspace { user: User, workspace: Workspace } keeps both models intact instead of flattening workspace fields into the user. Good names like UnverifiedEmail, PendingInvite, and BillingAddress tell you exactly what fields belong. If you see a phone_number field on BillingAddress, you know something went wrong.

Values with identical shapes can represent completely different domain concepts: { id: "123" } might be a DocumentReference in one place and a MessagePointer in another, and if your functions just accept { id: String }, the code will accept either one without complaint. Brand types solve this by wrapping a primitive in a distinct type so the compiler treats them as separate: DocumentId(UUID) instead of a bare UUID. With branding in place, accidentally swapping two IDs becomes a syntax error instead of a silent bug that surfaces three layers deep.

Where Things Break

Breaks commonly happen when a semantic function morphs into a pragmatic function for ease, and then other places in the codebase that rely on it end up doing things they didn't intend. To solve this, be explicit when creating a function by naming it instead of by what it does, but by where it's used. The nature of their names should make it clear to other programmers in their names that their behavior is not tightly defined and should not be relied on for the internals to do an exact task, and make debugging regressions from them easier.

Models break the same way but slower. They start focused, then someone adds "just one more" optional field because it's easier than creating a new model, and then someone else does the same, and eventually the model is a loose bag of half-related data where every consumer has to guess which fields are actually set and why. The name stops describing what the data is, the fields stop cohering around a single concept, and every new feature that touches the model has to navigate states it was never designed to represent. When a model's fields no longer cohere around its name, that's the signal to split it into the distinct things it's been coupling together.
