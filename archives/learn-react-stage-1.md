title: Learn React - Stage 1 - The Official Tutorial
tags: journal
---
![react](http://formatjs.io/img/react.svg)
![lulz](https://pbs.twimg.com/media/Ca9AO0QWEAAH__n.jpg)

Going through The Official React Tutorial
===
_The tutorial can be found [here](https://facebook.github.io/react/docs/tutorial.html)_

__Tutorial app:__ Disqus comment box clone

__Tutorial Time:__ About __4 hours__

__Tutorial app features:__

Main:
- A view of all of the comments
- A form to submit a comment
- Hooks for you to provide a custom backend

Other:
- Optimistic commenting: comments appear in the list before they're saved on the server so it feels fast.
- Live updates: other users' comments are popped into the comment view in real time.
- Markdown formatting: users can use Markdown to format their text.

Steps:
---

Notes:
---
Setup & Removed example script:

- First run shows finished product
- No live reload



First Component:
- Component structure
  ```
  - CommentBox
    - CommentList
      - Comment
    -CommentForm
  ```
- Note that native HTML element names start with a lowercase letter, while custom React class names begin with an uppercase letter.
React is safe. We are not generating HTML strings so XSS protection is the default.
- React is safe. We are not generating HTML strings so XSS protection is the default.
- ReactDOM.render() instantiates the root component, starts the framework, and injects the markup into a raw DOM element, provided as the second argument.
- The `ReactDOM` module exposes DOM-specific methods, while `React` has the core tools shared by React on different platforms (e.g., `React Native`).
- It is important that ReactDOM.render remain at the bottom of the script for this tutorial. ReactDOM.render should only be called after the composite components have been defined.

Composing components:
- HTML components are regular React components, just like the ones you define, with one difference. The JSX compiler will automatically rewrite HTML tags to React.createElement(tagName) expressions and leave everything else alone. This is to prevent the pollution of the global namespace.

Using props:
- By surrounding a JavaScript expression in braces inside JSX (as either an attribute or child), you can drop text or React components into the tree.
- We access named __attributes__ passed to the component as keys on `this.props` and any __nested elements__ as `this.props.children`.

Component properties:
-This allows us to reuse the same code for each unique comment.

Adding Markdown:
- Using __marked__ library which takes Markdown text and converts it to raw HTML.(already including in `head`)
- We need to convert this.props.children from __React's wrapped text__ to a raw string that marked will understand so we explicitly call `toString()`.
- HTML string is rendered at first because React prevents XSS by preventing raw HTML from being render.
- A special API called with an attribute called `dangerouslySetInnerHTML` allows us to set html by returning an object with markup as an element named `__html` from a component method.
- Leave *code comments* out of JSX return statements

Hook up the data model:
- Data is pass in as an array(they will be pulled from the server later)
- First into the `CommentBox` as an prop attribute called `data`
- Then into `CommentList` through the `props` where it is iterated over to create `Comment` nodes that are returned.

Fetching from the server:
- Replace the `data` prop with `url` attribute pointing to api endpoint.
- This component is different from the prior components because it will have to re-render itself. The component won't have any data until the request from the server comes back, at which point the component may need to render some new comments.
- the code will not be working at this step.

Reactive state:
- So far, based on its props, each component has rendered itself __once__. props are __immutable__: they are passed from the parent and are *"owned"* by the parent.
- To implement interactions, we introduce mutable state to the component. `this.state`(instead of `this.props`) is private to the component and can be changed by calling `this.setState()`. When the state updates, the component re-renders itself.
- `render()` methods are written __declaratively__ as functions of `this.props` and `this.state`.
- Here, `componentDidMount` is a method called automatically by React after a component is rendered for the first time
- Polling(ajax), websockets can be used to get data and set each time by using this.setState() to replace initial value. This will trigger of the component re-render.

Adding new comments:
- With traditional DOM, the `input` elements don't keep track of their state after being rendered. The browser is the keeper of the state! Hence state of the view differs from that of the component.
- In React, components should always represent the state of the view and not only at the point of initialization hence this.state is used to save user's input as it is entered.
-  In our `<input>` elements, we set the value prop to reflect the state of the component and attach `onChange` handlers to them. These `<input>` elements with a value set are called controlled components.
-`onSubmit` is used on the form(is it an HTML5 thing?)
- Callbacks can be passed to children from parents as props for children to call and pass their data into.

Optimistic updates:
- Store temp variable with comment list revert to it when the update fails.
