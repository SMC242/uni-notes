# Motivation

I think it would be easier to show which problems each technology solves and how. Saying a library does something doesn't give an idea of how it feels to use it. I've tried to include examples as much as possible. I've also added a learning curve estimate to aid evaluating the cost of adding each library to our stack

Additionally, this can be a jumping-off point for documenting our tech decisions. Jesus will be very happy to see some documentation

# Frameworks

## React

React is a JavaScript framework that provides the rendering, templating, and state + reactivity layer. By default, it supports client-side rendering (CSR) only

It uses a templating system called JSX which co-locates the JavaScript and markup for a component. The basic unit of React is the component - you think of pages as a composition of small components. A component is a function that returns JSX

Learning curve: 3/5
- JSX is easy to learn
- There are some gotchas. For example:
	- Having to use `useMemo` to avoid running an expensive computation on every render
	- State changes are detected by comparing *references*, not values
- There are notable foot-guns
	- `useEffect` does 3 different things, none of which are intuitive. Thankfully, react server components cut out the main use case of that hook

### Examples

Here is a simplified example from the bins demo
```jsx
function BinList() {
    const bins = []; // Data fetching omitted

    return (
        <div className="container bin-list">
            <h1 className="heading">
                This is a visualisation of the contents of each bin
            </h1>
            {/* 
                Rendering a <Bin> component for each bin object.

                NOTE: `key` helps React's rendering process by acting as
                an ID for the element. This makes the diffing process easier
            */}
            {bins.map((bin) => (
                <div key={bin.name}>
                    <Bin name={bin.name} papers={bin.papers} />
                </div>
            ))}
        </div>
    );
}

// This component expects an object containing the keys "binName" and "papers"
function Bin({ binName, papers }) {
    /* 
        There must be a single parent element in JSX.
        It doesn't always make sense for this to be a real DOM element,
        so we use a fragment (<>...</>) instead. Fragments are compiled away
        */
    return (
        <>
            <h2 className="bin-heading">{binName}</h2>
            {papers.map((paper) => (
                <Paper key={paper.id} paper={paper} />
            ))}
        </>
    );
}

function Paper({ authors, title, lastUpdated }) {
    return (
        <div className="container paper-container">
            <h3 className="paper-title">{title}</h3>
            <p className="authors-list">{authors.join(", ")}</p>
            <span className="align-right date">
                {lastUpdated.toDateString()}
            </span>
        </div>
    );
}

```

Here is a React server component
```jsx
// RSCExample.jsx
import {Suspense} from "react";

// This is a React server component
async function RSCExample() {
	const data = await someIO();

	return <span>{data.value}</span>
}

// Parent.jsx
function Parent() {
	return (
		{/* A loading spinner will be shown until RSCExample finishes rendering */}
		<Suspense fallback={<LoadingSpinner />}>
			<RSCExample />
		</Suspense>
	)
}
```

### Documentation

React has just moved to new documentation, meaning that many tutorials are outdated as of 16/03/2023
- [New documentation](https://react.dev/)
- [Quickstart](https://react.dev/learn)
	- [How to structure pages in React](https://react.dev/learn/thinking-in-react)
	- [Official tutorial](https://react.dev/learn/describing-the-ui) <-- 4-page tutorial. I recommend reading the first 3 pages only

### See also
- [[#NextJS]]

## NextJS

NextJS is a meta-framework built on top of React. It provides what React does not:
- Rendering strategies selected per-page
	- Client-side rendering (JS is sent to the client, the client runs it to generate the page)
	- Server-side rendering (the JS is run on the server and the result is sent to the client to be re-hydrated)
	- Static site generation (runs the JS on the server to generate a static page)
	- Incremental Static Regeneration (content is generated periodically)
- A framework for making REST APIs
	- Supports `GET`, `POST`, `PUT`, and `PATCH`
	- Lightweight middleware support
- Routing
	- React does not provide a way to move between pages
	- NextJS uses a file-based router
- A `fetch` wrapper with aggressive caching
	- An easy HTTP client that gives you caching for free
- Image optimisation

Additionally, `create-react-app` [is no longer recommended](https://react.dev/learn/start-a-new-react-project) for starting new projects

Learning curve: 2/5
- Average learning curve for a fully-fledged framework
	- Lots of reading the docs to begin with

### Examples

Here is an example directory structure
```
app
├── manage-team
│  └── page.jsx
├── profile
│  └── [id]
│     └── page.jsx
├── review
│  └── page.jsx
├── layout.jsx
└── page.jsx
```

- A `page.jsx` is the template for the content of the page
- Directories become URL segments
	- E.G `manage-team` --> `https://my.app/manage-team`
	- Directories with `[param]` in their name are dynamic routes
	- `layout.jsx` is the layout that pages will be rendered inside
	- `app/page.jsx` is the index page (`https://my.app/`)

Here is a POST request handler that echoes the request body

```js
// app/api/echo/route.js
// This will be mapped to https://my.app/api/echo
import {NextRequest, NextResponse} from "next/server";

export async function POST(request: NextRequest) {
    const json = await request.json();
    if (!("message" in json))
	    return NextResponse.json(
            {},
            {
                status: 400,
                statusText: "A `message` is required in the request body",
            }
        );
    return NextResponse.json({ body: json.body.message });
}
```

### Documentation

NextJS recently changed the way their router and backend works. The new version is called the "app router". Be careful when you read tutorials because they may be using the old "page router"

- [App router struture](https://nextjs.org/docs/app/building-your-application/routing)
- [Quickstart](https://nextjs.org/docs)

## Tailwind.css

Tailwind is a utility class library. Think Bootstrap but more granular. Instead of writing stylesheets, you add classes that correspond to the styles you desire

Pros:
- Transparent styling - the styles of an element are in the markup
- No unused styles are sent to the client
	- Tailwind only generates classes that are found in the source code

Cons:
- Cluttered markup
	- See [[#Inline Fold]] for a solution
- Dynamically generated class names don't work
	- See [[#CLSX]]

Learning curve: 1/5
- You don't really learn Tailwind. You just Google what you want to to (E.G search "tailwind flexbox" or "tailwind list decoration")
- The naming conventions can take some getting used to

### Examples

```html
<main class="bg-slate-200">
    <div class="container flex flex-row p-5">
        <nav>
            <a href="/" class="border rounded-sm bg-purple-100">Home page</a>
        </nav>
        <h1 class="text-xl">Lecturer Strikes</h1>
        <article class="bg-slate-100 border">
            <p>Article content</p>
        </article>
    </div>
</main>

```

### Documentation

- [Home page](https://tailwindcss.com/)

### See also

- [[#CLSX]]
- [[#Inline Fold]]

### Alternatives

CSS modules are a good alternative if we don't want to use a CSS framework like Bootstrap or Tailwind. They are scoped stylesheets that you import into your JavaScript code. An ID is prepended to the class name to prevent name collisions. CSS modules are usually co-located with a component

```css
// button.module.css
.container {
    width: 100%;
}

.heading {
    text-transform: capitalize;
    font-weight: 500;
}

.content {
    /* Vomit-inducing theme */
    background-color: lime;
    color: magenta;
}

.aside {
    color: grey;
    font-style: italic;
}
```

```jsx
// button.jsx
import styles from "./button.module.css";

function CSSModuleExample() {
    return (
        <div className={styles.container}>
            <h1 className={styles.heading}>Very important heading</h1>
            <p className={styles.content}></p>
            <aside className={styles.aside}>I am running out ideas. Please forgive the low-effort example</aside>
        </div>
    )
} 
```

## Jest

This is the preferred unit-testing framework for [[#React]]

Learning curve: 2/5
- It's just like every other front-end testing library

### Examples

Here is a test for a [[#React]]  counter component
```jsx
import { render, fireEvent } from '@testing-library/react';
import '@testing-library/jest-dom/extend-expect';
import Counter from './Counter';

test('renders Counter component', () => {
  const { getByText } = render(<Counter />);
  
  // Check if the component renders with the initial count value
  const countElement = getByText(/Count:/);
  expect(countElement).toBeInTheDocument();

  // Simulate button clicks and check if the count updates accordingly
  const incrementButton = getByText('Increment');
  const decrementButton = getByText('Decrement');

  fireEvent.click(incrementButton);
  expect(countElement.textContent).toBe('Count: 1');

  fireEvent.click(decrementButton);
  expect(countElement.textContent).toBe('Count: 0');
});
```

It can also be used for testing business logic

### Documentation

- [Docs](https://jestjs.io/)

# Language

## NodeJS

NodeJS is a runtime that allows JavaScript to run on the server. It provides the usual APIs you'd expect from a backend language (`fs` for the file system, `crypto` for cryptography). The [[#NextJS]] server runs on NodeJS in order to serve responses to browsers. A bundler like [Webpack](https://webpack.js.org/) or [Vite](https://vitejs.dev/) will combine the generated JS, CSS, and HTML into a bundle and send it back to the browser

It comes with a package manager (`npm`) out of the box. It's like Python's `pip`, except dependencies are sand-boxed instead of global

Learning curve: 0/5
- It's just JavaScript running in a different place
- `npm` is just like any other package manager
	- There are a few other package managers in the space like `yarn` and `pnpm` which offer big performance improvements

### Documentation

- [Docs](https://nodejs.org/dist/latest-v21.x/docs/api/)

## TypeScript

TypeScript is a super-set of JavaScript that is transpiled to JavaScript. It adds a type system and some features such as decorators. It has very good type-inference, meaning that you often write types once and let them propagate through your program. It helps you to write self-documenting code and type errors at compile-time. It also aids refactoring by not allowing you to compile until your program is sane

Learning curve: 3/5
- Since it's a super-set of JavaScript, it can be gradually adopted
- Basic TypeScript will be very familiar coming from a Java background
- Some errors are very intimidating
	- See [[#Pretty TS Errors]]
- Advanced TypeScript can seem like wizardry

### Examples

Here are the basic features:
```ts
// Java-style static typing
function add(x: number, y: number): number {
    return x + y;
}

const result: number = add(1, 2);

// You could just as easily infer these types
const add2 = (x: number, y: number) => x + y;
//      ^? const add2: (x: number, y: number) => number
// NOTE: these arrow comments are TwoSlash queries.
// They reveal the type of the variable above it.
// Hovering over the variable in your IDE will show the type as well
const result2 = add2(1, 2);
//          ^? const result2: number

/*
    TypeScript uses structural typing, which means
    that if two objects look the same, they are the same
*/
interface Paper {
    title: string;
    authors: string[];
    abstract: string;
    // A function that accepts a string and a boolean returning nothing
    addVote: (reviewerID: string, vote: boolean) => void;
    // An async function that returns a string
    fullText: () => Promise<string>;
}

const paper: Paper = {
    title: "; drop table papers; --",
    authors: ["Bobby Tables", "Sally Fields"],
    abstract: "A brief history of the BEAM VM",
    addVote: (reviewerID: string, vote: boolean) => {
        // Do something
    },
    fullText: async () => {
        // Do something
        return "The full text of the paper";
    },
};

// Classes are also types, but interfaces are preferred
class PaperClass {
    title: string;
    authors: string[];
    private secrets: string;

    constructor(title: string, authors: string[]) {
        this.title = title;
        this.authors = authors;
    }

    addVote(reviewerID: string, vote: boolean) {
        // Do something
    }

    async fullText() {
        // Do something
        return "The full text of the paper";
    }
}

// TypeScript has enums
enum Vote {
    REJECT = 0,
    ACCEPT = 1,
}

// They have weird runtime behaviour. Always prefer discriminated unions
type Vote2 = "REJECT" | "ACCEPT";

// Generics
const take = <T>(n: number, xs: T[]): T[] => xs.slice(0, n);
const result3 = take(2, [1, 2, 3, 4, 5]);
//       ^? const result3: number[]

```

### Documentation

- [Documentation](https://www.typescriptlang.org/)
- [Official tutorial](https://www.typescriptlang.org/docs/handbook/2/basic-types.html) N.B this tutorial isn't very good. I just learned TypeScript by using it

### See also

- [[#TypeScript Pretty Errors]]
- [[#TwoSlash]]
- [[#Zod]]

# Libraries

## CLSX

A JavaScript library for conditionally concatenating classes. This solves two problems:
1. Using string interpolation is clunky
2. Tailwind's pre-processor doesn't understand dynamically generated class names. See [[#CLSX#Examples]]

Learning curve: 0/5
- This library only provides one function

### Examples

In this example, the background will be set to green if `isActive = true`
```jsx
const MyComponent = () => {
  const [isActive, setIsActive] = useState(false);

  return (
    <div>
      <button onClick={() => setIsActive(!isActive)}>Toggle</button>

      <div className={clsx('bg-blue-500', 'text-white', 'p-4', { 'bg-green-500': isActive })}>
        {isActive ? 'Active' : 'Inactive'} Box
      </div>
    </div>
  );
};

```

If you did this with string interpolation, Tailwind's pre-processor would not know which classes to generate and you would likely see a white background instead. This method also doesn't scale well as you add more classes

```tsx
<div className={`bg-${isActive ? "green" : "blue"}-500 text-white p-4`}>
```

### Documentation

- [NPM Registry package](https://www.npmjs.com/package/clsx)

## Zod

Zod is a TypeScript schema library. It lets you specify a schema and easily check if an object fits it. This prevents having to write type assertions in data fetching functions. Reducing friction in this area reduces the temptation to write unsafe code (E.G cast the response or set it to `any`)

### Examples

The problem: ensuring that requests and responses follow the expected schema
```ts
// app/api/papers/route.ts
import { NextRequest, NextResponse } from "next/server";

type PaperUpload = {
    title: string;
    authors: string[];
    license?: string;
};

// A type assertion.
// If this returns true, TypeScript can narrow the type
// of `x` to `PaperUpload`
const isPaperUpload = (x: unknown): x is PaperUpload =>
    x !== undefined && typeof x.title === "string" && Array.isArray(x.authors);

export async function PUT(request: NextRequest) {
    // TypeScript can't know what the type of the body is at runtime
    let body = await request.json();
    //   ?^ let body: unkown

    // The proper thing to do:
    if (!isPaperUpload(body)) return NextResponse.json({}, { status: 400 });
    // What actually happens (because writing type assertions is cumbersome)
    body = (await request.json()) as PaperUpload;
    // ^ This is unsafe because you're not checking if the shape is correct

    // Insert the paper into the database
    // ...
    return NextResponse.json(body);
}
```

Using Zod instead:

```ts
import z from "zod";

const PaperUploadSchema = z.object({
    title: z.string(),
    authors: z.array(z.string()),
    license: z.string().optional(),
});

// Extract the type of the schema
type PaperUpload2 = z.infer<typeof PaperUploadSchema>;
//     ^? type PaperUpload2 = { title: string; authors: string[]; license?: string | undefined; }

export async function PUT2(request: NextRequest) {
    let body = await request.json();
    //   ?^ let body: unkown
    const result = PaperUploadSchema.safeParse(body);
    if (!result.success) return NextResponse.json({}, { status: 400 });
    // Insert the paper into the database
    // ...
    return NextResponse.json(result.data);
}

```

### Documentation

- [Docs](https://zod.dev/?id=basic-usage)

## Tanstack Query

Tanstack Query is a data-fetching library for a variety of front-end frameworks. It handles:
- Fetching
	- No need for `useEffect` :)
- Caching and cache invalidation
- Pagination
- Re-fetching in the background
- Easy error handling

Getting all of these things right without bugs is difficult

### Examples

Due to the relatively large amount of set-up code, it's best to just look at the [example in the documentation](https://tanstack.com/query/latest/docs/react/overview#enough-talk-show-me-some-code-already)

### Documentation

- [Quickstart](https://tanstack.com/query/latest/docs/react/quick-start)

## NextAuth

TODO


# Tooling

## ESLint

A linter (static analyser) for JavaScript/TypeScript and JSX. It makes sure your code style is good and warns you about mistakes (E.G unchecked index access, ). It also provides very good auto-fixes - most problems can be fixed automatically

We would run this in our CI as well as using it as an editor extension

Learning curve: 2/5
- Some warnings can be hard to understand

### Documentation

- [Docs](https://eslint.org/)

## Prettier

A formatter for all common file formats in the web development space. It's very helpful to have a common formatting config in order to minimise arguments about code style. It also prevents the issue of file diffs being obfuscated by differing formatting styles

This would be run as an editor extension. We would agree on a single config that would be stored in our project repository

Learning curve: 0/5
- It automatically formats your files - nothing to learn

### Documentation

- [Docs](https://prettier.io/docs/en/)

## TwoSlash

This is a helpful IDE extension that has been used in many of the TypeScript examples. It lets you check the type of a variable. This is sometimes better than hovering over the variable, particularly when debugging because you can see if the type changes without hovering

This wouldn't be required. It's just worth mentioning

[VS Code extension](https://marketplace.visualstudio.com/items?itemName=Orta.vscode-twoslash-queries)

Learning curve: 0/5
- It's just a comment format

### Examples

From the docs:
![TwoSlash example](https://github.com/orta/vscode-twoslash-queries/raw/HEAD/vscode-twoslash.png)

## Pretty TS Errors

It converts TypeScript errors to a more readable format with syntax highlighting. It usually gives an explanation of the error too. TypeScript errors can be monstrous, so much so that they hide the cause of the error

I highly recommend this for people new to TypeScript

[VS Code extension](https://open-vsx.org/extension/yoavbls/pretty-ts-errors)

Learning curve: -2/5
- Makes learning TypeScript easier
### Examples

From the docs:
![After](https://github.com/yoavbls/pretty-ts-errors/raw/main/assets/this.png)
![Before](https://github.com/yoavbls/pretty-ts-errors/raw/main/assets/instead-of-that.png)

# Database

TODO: someone who knows databases please fill this part in

# Infrastructure
TODO: flesh this out once the systems design is done

- Docker: containers for easy deployments
- RabbitMQ: a message queue
- Azure Functions: serverless functions on Microsoft Azure
