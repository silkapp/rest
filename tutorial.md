---
title: Tutorial - Writing a simple REST API
layout: default

---

# Writing a simple REST API

This tutorial is an introduction to writing REST APIs using the [rest
packages](https://github.com/silkapp/rest). It will cover defining the API, running it in a web
framework, generating documentation, and generating and running API client libraries.

The running example we'll use is an API for a blog. This means our objects will be things like
posts, users and comments. It's loosely based on the example code in the
[rest-example](https://github.com/silkapp/rest/tree/master/rest-example) repository.

## Defining a resource

The basic building block of a REST API is a *resource*. You define one using the `Resource` type
from rest-core. This data type represents a single resource, like a blog post or a user. The type
looks like this:

``` haskell
data Resource m s sid mid aid where
...
```

The first two type parameters represent the context that the handlers for this resource will run in.
The first represents the context you get from your parent resource. The second one is the context
you get after a single resource has been identified. It can be used to pass data to subresources.
We'll see an example of this soon. The last three type parameters are identifiers for this resource:
one for a single item, one for listings, and one for top-level actions. We'll get to these later.

Let's define a resource for blog posts. The easiest way to create a resource is using one of the
smart constructors: `mkResourceId`, `mkResourceReader` and `mkResourceReaderWith`. Which one you use
depends on what the types `m` and `s` are in your resource. Since this is a top level resource and
we don't do anything special, we'll have the first one be `IO`. We'll define the second, which is
the context for subhandlers, to contain the `Title` of the post using `ReaderT Title IO`. This means
we'll use `mkResourceReader`:

``` haskell
module Api.Post (resource) where

import Rest
import qualified Rest.Resource as R

type Title = String

resource :: Resource IO (ReaderT Title IO) Title () Void
resource = mkResourceReader
  { R.name   = "post"
  , R.schema = withListing () $ named [("title", singleBy id)]
  , R.list   = const list
  , R.get    = Just get
  }
```

The first field we update just sets the string that will be used for this resource in urls. The
second field is more interesting. It defines the routes that are available on this resource. In this
case, we define a top level listing, and a way to get a single post by title. The listing will be
available on the path `/post`, and the indiviual items on paths like `/post/title/<title>`.

The argument to `withListing` is the identifier for the type of listing, corresponding to the type
parameter `mid` that we saw before. Later we'll see when you would use a different type here.

The `named` function takes a list of named things: either single resources (`single*`), listings
(`listing*`) or actions (`action`). In this case we use `singleBy id`, which just uses the variable
part of the url (the title) directly as the `sid` type. We'll see different ways to use this type
later.

Finally we define the handlers for getting a single resource, and getting a listing. These contain
the actual implementation code that is specific to your API. Let's look at how we can implement
these. The handler to get a single post has type `Handler IO`. We create a `Handler` using smart
constructors again. In this case we'll use `mkIdHandler`, which will give us the input (from the
request body) which in this case is ignored, and the resource identifier (the post title).

``` haskell
data Post = Post { title :: Title, content :: String }
instance XmlPickler Post where ...
instance ToJSON     Post where ...
instance FromJSON   Post where ...
instance JSONSchema Post where ...

get :: Handler (ReaderT Title IO)
get = mkIdHandler xmlJsonO $ \_ title -> liftIO $ readPostFromDb title

readPostFromDb :: Title -> IO Post
```

The `mkIdHander` constructor takes two arguments. The first argument is the I/O dictionary. It
describes the types of inputs and outputs accepted by your handler. In this case we allow output in
either XML or JSON format. To do this, our output type `Post` needs instances for the `XmlPicker`
and `ToJSON`, `FromJSON` and `JSONSchema` classes.

The code for listings is very similar:

``` haskell
list :: ListHandler IO
list = mkListing xmlJsonO $ \range -> lift $ readPosts (offset range) (count range)

readPosts :: Int -> Int -> IO [Post]
```

Here we use the `mkListing` smart constructor. This gives us a `Range` value as an argument to the
handler, which is passed by two GET parameters `offset` and `count`. We then query and return a list
of posts. The `ListHandler` actually requires the returned type to be a list; returning a non-list
value will cause a type error.

### Other kinds of handlers

In addition to `get` and `list`, there are several more fields for defining handlers on your
resources:

* `statics`: These are top-level POST actions. An example would be `/user/login`. They are
  identified by the fifth type parameter in the `Resource` type. In the schema, they are created by
  `action`.
* `update`: Allows creating and updating an identified resource. This is done with a PUT to the same
  url as the single getter.
* `delete`: Allows deleting an identified resource. This is done with a DELETE to the same url as
  the single getter.
* `create`: Allows creating a new resource. This is done with a POST to the root of the resource.
* `actions`: POST Actions on an identified resource. An example would be `/user/id/1/signout`.
* `selects`: Small subobjects of an identified resource. These could be a singleton subresource, but
  sometimes having them on their parent is easier.

### Error handling in handlers

The body of a handler of type `Handler m` doesn't run directly in `m`. Instead, it runs in an
`ErrorT (Reason e) m`. This allows handlers to throw errors with `throwError`. The `Reason` data
type contains common errors, like `NotFound` and `NotAllowed`. Additionally, you can instantiate the
type variable `e` to your own error data type. If you do this, you need to specify the serialization
format(s) of your data type in a dictionary, just like we did for the input and output.

As an example, we can have the `get` handler sometimes throw an error:

``` haskell
data CustomError = CustomError
instance XmlPickler CustomError where ...
instance ToJSON     CustomError where ...
instance FromJSON   CustomError where ...
instance JSONSchema CustomError where ...

get :: Handler (ReaderT Title IO)
get = mkIdHandler (xmlJsonE . xmlJsonO) $ \_ title ->
  case title of
    "notfound" -> throwError NotFound
    "custom"   -> throwError $ domainReason (const 500) CustomError
    _          -> liftIO $ readPostFromDb title
```

Here we throw two errors in the body of the handler. The `NotFound` error comes from 'rest-types'.
We also define a custom error, that we throw with `domainReason`. The first argument gives a
function mapping the custom error type to an HTTP status code.


## Composing resources into an API

Now that we have a resource, we want to combine it with other resources into an API. Let's assume we
also wrote a resource for user objects. We can now combine these two into an API like this:

``` haskell
import Rest.Api
import qualified Api.Post as Post
import qualified Api.User as User

blog :: Router IO IO
blog = root -/ user
            -/ post
  where
    user = route User.resource
    post = route Post.resource
```

If we have subresources, for example comments on a blog post, we can add those to the router as
well:

``` haskell
blog = root -/ user
            -/ post --/ comment
```

All combinators for combining resources into routes (`-/`, `--/`, `---/` etc.) are actually the same
function. They just have different precedences to make composing them without parentheses possible.

## Versioning your API

To turn the `Router` we just made into a runnable API, there's one more step to take: we have to add
a verion to our API. All APIs build with the 'rest' packages have a version string that is prepended
to all urls. It contains three components, which have the same semantics as the [Haskell package
versioning policy] and are also similary to [semantic versioning] in general: a change in the first two
components indicates a breaking change, where clients of your API would have to change their code.
The last component indicates an incremental change that doesn't break API clients.

An actual API is a list of versioned routers. This means that if you add a new version, clients can
still keep accessing the old version until they upgrade their code. For minor upgrade, if a client
requests version `x.y.z` we will serve `x.y.w` where `w` is the largest available version larger
than `z`.

``` haskell
api :: Api IO
api = [(mkVersion 1 0 0, Some1 blog)]
```

## Running it

You can run your API in several different web frameworks. At this moment there are packages for
[happstack](http://hackage.haskell.org/package/rest-happstack),
[snap](http://hackage.haskell.org/package/rest-happstack) and
[wai](http://hackage.haskell.org/package/rest-wai). In this tutorial I'll show how to run the API in
happstack, but code for other frameworks is very similar.

To run your api, you need to convert from the monad your API is running in (`IO` in our case) to the
monad used for the web framework (`ServerPartT IO` for happstack). In this case, that's just
`liftIO`. Then we call the `apiToHandler'` function which gives us a `ServerPartT IO Response` which
we can use in happstack as you normally would:

``` haskell
handle :: ServerPartT IO Response
handle = apiToHandler' liftIO api
```

And that's it! You now have a runnable REST API supporting both XML and JSON. Next, we'll look at
how to generate documentation and client libraries.

## Generating documentation and client code

The 'rest-gen' package contains code to generate documentation and client code from your Haskell
APIs. The easiest way to use this package is to create an executable to generate your documentation
and client code. We provide a set of command line flags to customize generation, and a configurable
function to generate the code:

``` haskell
main = do
  config <- Gen.configFromArgs "rest-example-gen"
  Gen.generate config "RestExample" Api.api [] [] []
```

The `configFromArgs` function takes the name of your executable, and parses a set of command line
options configuring the code generation. Using this configuration, you call `generate` to generate
the code. In addition to the configuration, you pass a name used for the generated API object in
e.g. Javascript, and the actual API code.

When running your generetion executable, you can now pass several flags:

```
  -d URLROOT   --documentation=URLROOT  Generate API documentation, available under the provided URL root.
  -j           --javascript             Generate Javascript bindings.
  -r           --ruby                   Generate Ruby bindings.
  -h           --haskell                Generate Haskell bindings.
  -s LOCATION  --source=LOCATION        The location of additional sources.
  -t LOCATION  --target=LOCATION        The target location for generation.
  -v VERSION   --version=VERSION        The version of the API under generation. Default latest.
  -p           --hide-private           Generate API for the public, hiding private resources. Not default.
```

There are three additional arguments that you can pass to `generate` to customize it further. The
first is a list of modules that are added to the `exposed-modules` for the generated Haskell client.
This can be useful if you add some custom hand-written modules to your automatically generated
client. The second one contains a list of extra imports added to every generated modules. The third
is a list of rewrites to perform on the imported modules, replacing the first by the second. This
can be needed for packages that have `Internal` modules, to rewrite those imports to the
non-internal versions.

### Generating documentation

To generate documentation, run the generator passing `--documentation=<root>`, where `<root>` is the
root that your API runs on. This is used to generate links between resources. You also need to pass
`--source=<template-dir>` pointing to a directory of templates. A default set is included in the
'rest-gen' package, in `files/Docs/`. This will output documentation files to `./docs`. You can
change the output directory using `--target=<output-dir>`.

You can use the generation code to produce static documentation files and serve those, but there is
another option. The API server running your API can also dynamically serve the corresponding
documentation as well. Currently this is only supported for the happstack driver, but it should be
easy to implement for other frameworks as well.

To serve the documentation, just call `apiDocsHandler` with a root url where the documentation will
be served, a template directory and your API. This gives you a happstack handler that you can mount
in your server where you want.

### Generating a Haskell client

The same generation executable can also build a Haskell client. Pass `--haskell` to the program,
which will output client code to `./client`. You can change the output directory using
`--target=<output-dir>`.

This will generate a client library. It has a module for each resource in the API, as well as a
cabal file exposing these modules and depending on a few needed libraries. You will have to add your
own dependencies as well to get your domain types in scope. It is a good design to have three
packages: 'something-api' containing the runnabe API, 'something-client' for talking to this API,
and 'something-types' which contains shared types between the two.

Let's look at the generated code for the 'post' API resource:

``` haskell
list :: ApiStateC m
     => [(String, String)]
     -> m (ApiResponse () (List Post))

byId :: ApiStateC m
     => Int
     -> m (ApiResponse () Post)
```

For each action ('list' and 'get') a client function was created. These run in the context of
`ApiStateC`. This type is defined in the 'rest-client' package. It represents the context to make
HTTP calls in: it has a cookie jar to track (login) cookies, and the host and port to connect to the
API. The simplest instance is the `ApiT` transformer, which you can easily run:

``` haskell
run :: String -> ApiT IO a -> IO a
```

You pass in the url of the API as the first argument (port 80 is used) and then runs the API calls
which are the second argument. To list the posts, we would do something like:

``` haskell
run "my.local.example" (Post.list [])
```

The list argument can contain query parameters, like 'count' and 'limit'. Running this gives us back
an `ApiResponse` containing a `List Post`. The `ApiResponse` contains information about the
response: the response code, the headers and the body. The body is either an error, or the actual
result. That result is a `List`, which in addition to the actual results also contains a count and
an offset. This type is defined in the 'rest-types' package.

### Generating a Javascript client

To generate a Javascript library, pass `--javascript` to the program. This will output the client
code to standard out. To output to a file instead, you can use `--target=<output-file>`.

To use the client library, include it and [jQuery](http://jquery.com) in a page. Note that due to
cross domain restrictions, in browsers you can only access the API if it runs on the same domain as
your client application, or if you set the [appropriate
headers](http://en.wikipedia.org/wiki/Cross-origin_resource_sharing).

To use the client, first instantiate the API object:

``` javascript
var api = new RestExampleApi(apiHost);
```

This object contains properties for all top level resources in your API. These properties contain
functions for calling actions on this resource. The functions take a success and an error handler,
but they also return a [jQuery Deferred](http://api.jquery.com/category/deferred-object/) of the
AJAX call, so you can also chain using e.g. `.then()`. Calls that need input data take it as the
first argument. After the callbacks, you can also pass additional query parameters.

For example, to list all posts and print them to the console, we could do:

``` javascript
api.Post.list().then(function (posts) { console.log(posts); });
```
