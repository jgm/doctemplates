# doctemplates

This is the text templating system used by pandoc.  Its basic
function is to fill holes in a template in a "Context" that
provides values for variables.  Control structures are provided
to test that a variable has a non-blank value and to iterate
over the items of a list.  Partials---that is, subtemplates
defined in different files---are also supported.

Templates may be rendered to lazy or strict `Text`, `String`,
or a doclayout `Doc`.  (Using a `Doc` allows rendered documents
to wrap flexibly on breaking spaces.)  A Context can be
constructed manually or an aeson `Value` may be used.

Unlike the various HTML-centered template engines,
doctemplates is output-format agnostic, so no automatic escaping
is done on interpolated values.  Values are assumed to be
escaped properly in the Context.

## Example of use

``` haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Aeson
import Text.DocTemplates

data Employee = Employee { firstName :: String
                         , lastName  :: String
                         , salary    :: Maybe Int }
instance ToJSON Employee where
  toJSON e = object [ "name" .= object [ "first" .= firstName e
                                       , "last"  .= lastName e ]
                    , "salary" .= salary e ]

template :: Text
template = "$for(employee)$Hi, $employee.name.first$. $if(employee.salary)$You make $employee.salary$.$else$No salary data.$endif$$sep$\n$endfor$"

main :: IO ()
main = do
  res <- compileTemplate "mytemplate.txt" template
  case res of
         Left e    -> error e
         Right t   -> T.putStrLn $ renderTemplate t $ object
                        ["employee" .=
                          [ Employee "John" "Doe" Nothing
                          , Employee "Omar" "Smith" (Just 30000)
                          , Employee "Sara" "Chen" (Just 60000) ]
                        ]
```

## Delimiters

To mark variables and control structures in the template,
either `$`...`$` or `${`...`}` may be used as delimiters.
The styles may also be mixed in the same template, but the
opening and closing delimiter must match in each case.  The
opening delimiter may be followed by one or more spaces
or tabs, which will be ignored. The closing delimiter may
be followed by one or more spaces or tabs, which will be
ignored.

To include a literal `$` in the document, use `$$`.

## Comments

Anything between the sequence `$--` and the end of the
line will be treated as a comment and omitted from the output.

## Interpolated variables

A slot for an interpolated variable is a variable name surrounded
by matched delimiters.  Variable names must begin with a letter
and can contain letters, numbers, `_`, `-`, and `.`.  The
keywords `it`, `if`, `else`, `endif`, `for`, `sep`, and `endfor` may
not be used as variable names. Examples:

```
$foo$
$foo.bar.baz$
$foo_bar.baz-bim$
$ foo $
${foo}
${foo.bar.baz}
${foo_bar.baz-bim}
${ foo }
```

The values of variables are determined by a JSON object that is
passed as a parameter to `renderTemplate`.  So, for example,
`title` will return the value of the `title` field, and
`employee.salary` will return the value of the `salary` field
of the object that is the value of the `employee` field.

- If the value of the variable is a JSON string, the string will
  be rendered verbatim.  (Note that no escaping is done on the
  string; the assumption is that the calling program will escape
  the strings appropriately for the output format.)
- If the value is a JSON array, the values will be concatenated.
- If the value is a JSON object, the string `true` will be
  rendered.
- If the value is a JSON number, it will be rendered as an
  integer if possible, otherwise as a floating-point number.
- If the value is a JSON boolean, it will be rendered as `true`
  if true, and as the empty string if false.
- Every other value will be rendered as the empty string.

The value of a variable that occurs by itself on a line
will be indented to the same level as the opening delimiter of
the variable.

## Conditionals

A conditional begins with `if(variable)` (enclosed in
matched delimiters) and ends with `endif` (enclosed in matched
delimiters).  It may optionally contain an `else` (enclosed in
matched delimiters).  The `if` section is used if
`variable` has a non-empty value, otherwise the `else`
section is used (if present).  (Note that even the
string `false` counts as a true value.) Examples:

```
$if(foo)$bar$endif$

$if(foo)$
  $foo$
$endif$

$if(foo)$
part one
$else$
part two
$endif$

${if(foo)}bar${endif}

${if(foo)}
  ${foo}
${endif}

${if(foo)}
${ foo.bar }
${else}
no foo!
${endif}
```

The keyword `elseif` may be used to simplify complex nested
conditionals.  Thus

```
$if(foo)$
XXX
$elseif(bar)$
YYY
$else$
ZZZ
$endif$
```

is equivalent to

```
$if(foo)$
XXX
$else$
$if(bar)$
YYY
$else$
ZZZ
$endif$
$endif$
```

## For loops

A for loop begins with `for(variable)` (enclosed in
matched delimiters) and ends with `endfor` (enclosed in matched
delimiters.  If `variable` is an array, the material inside
the loop will be evaluated repeatedly, with `variable` being set
to each value of the array in turn.  If the value of the
associated variable is not an array, a single iteration will be
performed on its value.

Examples:

```
$for(foo)$$foo$$sep$, $endfor$

$for(foo)$
  - $foo.last$, $foo.first$
$endfor$

${ for(foo.bar) }
  - ${ foo.bar.last }, ${ foo.bar.first }
${ endfor }
```

You may optionally specify a separator between consecutive
values using `sep` (enclosed in matched delimiters).  The
material between `sep` and the `endfor` is the separator.

```
${ for(foo) }${ foo }${ sep }, ${ endfor }
```

Instead of using `variable` inside the loop, the special
anaphoric keyword `it` may be used.

```
${ for(foo.bar) }
  - ${ it.last }, ${ it.first }
${ endfor }
```

## Partials

Partials (subtemplates stored in different files) may be
included using the syntax

```
${ boilerplate() }
```

The partials are obtained using `getPartial` from
the `TemplateMonad` class.  This may be implemented
differently in different monads.  The path passed
to `getPartial` is computed on the basis of the
original template path (a parameter to `compileTemplate`)
and the partial's name.  The partial's name is substituted
for the *base name* of the original template path
(leaving the original template's extension), unless
the partial has an explicit extension, in which case
this is kept.  So, with the `TemplateMonad` instance
for IO, partials will be sought in the directory
containing the main template, and will be assumed
to have the extension of the main template.

Partials may optionally be applied to variables using
a colon:

```
${ date:fancy() }

${ articles:bibentry() }
```

If `articles` is an array, this will iterate over its
values, applying the partial `bibentry()` to each one.
So the second example above is equivalent to

```
${ for(articles) }
${ it:bibentry() }
${ endfor }
```

Note that the anaphoric keyword `it` must be used when
iterating over partials.  In the above examples,
the `bibentry` partial should contain `it.title`
(and so on) instead of `articles.title`.

Final newlines are omitted from included partials.

Partials may include other partials.  If you exceed
a nesting level of 50, though, in resolving partials,
the literal `(loop)` will be returned, to avoid infinite loops.

A separator between values of an array may be specified
in square brackets, immediately after the variable name
or partial:

```
${months[, ]}$

${articles:bibentry()[; ]$
```

The separator in this case is literal and (unlike with `sep`
in an explicit `for` loop) cannot contain interpolated
variables or other template directives.

## Breakable spaces

When rendering to a `Doc`, a distinction can be made between
breakable and unbreakable spaces.  Normally, spaces in the
template itself (as opposed to values of the interpolated
variables) are not breakable, but they can be made breakable
in part of the template by using the `+reflow` keyword (ended
with `-reflow`).

```
${+reflow}This long line may break if the document is rendered
with a short line length.${-reflow}
```

The `+` keyword has no effect when rendering to `Text`
or `String`.

## Nesting

As noted above, the value of a variable that occurs by itself on
a line will be indented to the same level as the opening
delimiter of the variable.

In addition, any part of a template can be marked explicitly for
indented rendering, using the `+nest` keyword (o start nesting at
the column where it appears) and `-nest` to stop nesting.

Example:

```
$for(article)$
- $+nest$$article.author$, "$article.title$," in $article.book$
  ($article.year$).$-nest$
$endfor$
```

