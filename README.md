# doctemplates

This is the templating system used by pandoc.  It was formerly
be a module in pandoc. It has been split off to make it easier
to use independently.

Example:

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
main = case compileTemplate template of
         Left e    -> error e
         Right t   -> T.putStrLn $ renderTemplate t $ object
                        ["employee" .=
                          [ Employee "John" "Doe" Nothing
                          , Employee "Omar" "Smith" (Just 30000)
                          , Employee "Sara" "Chen" (Just 60000) ]
                        ]
```

A slot for an interpolated variable is a variable name surrounded
by dollar signs.  To include a literal `$` in your template, use
`$$`.  Variable names must begin with a letter and can contain letters,
numbers, `_`, `-`, and `.`.

The values of variables are determined by a JSON object that is
passed as a parameter to `renderTemplate`.  So, for example,
`title` will return the value of the `title` field, and
`employee.salary` will return the value of the `salary` field
of the object that is the value of the `employee` field.

The value of a variable will be indented to the same level as the
variable.

A conditional begins with `$if(variable_name)$` and ends with `$endif$`.
It may optionally contain an `$else$` section.  The if section is
used if `variable_name` has a non-null value, otherwise the else section
is used.

Conditional keywords should not be indented, or unexpected spacing
problems may occur.

The `$for$` keyword can be used to iterate over an array.  If
the value of the associated variable is not an array, a single
iteration will be performed on its value.

You may optionally specify separators using `$sep$`, as in the
example above.

Anything between the sequence `$--` and the end of the line
will be treated as a comment.
