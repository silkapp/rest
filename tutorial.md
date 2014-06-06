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
For now, let's just assume they are both IO. The last three type parameters are identifiers for this
resource: one for a single item, one for listings, and one for top-level actions. We'll get to these
later.

Let's define a resource for blog posts. The easiest way to create a resource is using one of the
smart constructors: `mkResourceId`, `mkResourceReader` and `mkResourceReaderWith`. Which one you use
depends on what the types `m` and `s` are in your resource. Since we define both to be `IO`, we'll
use `mkResourceId`:

``` haskell
module Api.Post (resource) where

import Rest
import qualified Rest.Resource as R

type Title = String

resource :: Resource IO IO Title () Void
resource = mkResourceId
  { R.name   = "post"
  , R.schema = withListing () $ named [("title", singleBy id)]
  , R.list   = list
  , R.get    = get
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
instance XmlPickler Post
instance ToJSON Post
instance FromJSON Post
instance JSONSchema Post

get :: Handler IO
get = mkIdHandler xmlJsonO $ \_ title -> lift $ readPostFromDb title

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

#### Error reporting
#### Different kinds of handlers
## Documentation and clients
### Generation
### Serving the documentation
### Using the Javascript client
### Using the Haskell client
