module namespace pml='http://www.tei-c.org/tei-simple/models/tls.odd/module';

import module namespace m='http://www.tei-c.org/tei-simple/models/tls.odd' at '/db/apps/tls-text/transform/tls-web.xql';

(: Generated library module to be directly imported into code which
 : needs to transform TEI nodes using the ODD this module is based on.
 :)
declare function pml:transform($xml as node()*, $parameters as map(*)?) {

   let $options := map {
       "styles": ["../generated/tls.css"],
       "collection": "/db/apps/tls-text/transform",
       "parameters": $parameters
   }
   return m:transform($options, $xml)
};