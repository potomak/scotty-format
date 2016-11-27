# scotty-format

[scotty-format](https://github.com/potomak/scotty-format) is a helper for the
[Scotty](https://github.com/scotty-web/scotty) web framework that helps you
defining different response formats based on the request's `Accept` header
value.

The functional inspiration for this helper comes from how Rails handles
different response formats. In Rails you can implement any action to respond
with different formats based on a request `Accept` header value.

Inspiration for the implementation of this helper comes from
[scotty-resource](https://github.com/taphu/scotty-resource), another helper for
Scotty that lets you define REST resources easily by following HTTP strictly.

## Usage example

Checkout the
[examples](https://github.com/potomak/scotty-format/tree/master/examples)
directory.
