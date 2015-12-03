import module namespace m='http://www.tei-c.org/tei-simple/models/tls.odd' at '/db/apps/tls-text/transform/tls-web.xql';

declare variable $xml external;

declare variable $parameters external;

let $options := map {
    "styles": ["../generated/tls.css"],
    "collection": "/db/apps/tls-text/transform",
    "parameters": $parameters
}
return m:transform($options, $xml)