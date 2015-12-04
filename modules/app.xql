xquery version "3.0";

module namespace app="http://existsolutions.com/apps/TLS/texts/templates";

import module namespace templates="http://exist-db.org/xquery/templates" ;
import module namespace config="http://existsolutions.com/apps/TLS/texts/config" at "config.xqm";
import module namespace kwic="http://exist-db.org/xquery/kwic" at "resource:org/exist/xquery/lib/kwic.xql";
import module namespace pages="http://existsolutions.com/apps/TLS/texts/pages" at "pages.xql";

declare namespace expath="http://expath.org/ns/pkg";
declare namespace tei="http://www.tei-c.org/ns/1.0";
declare namespace functx="http://www.functx.com";

(:modified by applying functx:escape-for-regex() :)
declare %private function functx:number-of-matches 
  ( $arg as xs:string? ,
    $pattern as xs:string )  as xs:integer {
       
   count(tokenize(functx:escape-for-regex(functx:escape-for-regex($arg)),functx:escape-for-regex($pattern))) - 1
 } ;

declare %private function functx:contains-any-of
  ( $arg as xs:string? ,
    $searchStrings as xs:string* )  as xs:boolean {

   some $searchString in $searchStrings
   satisfies contains($arg,$searchString)
 } ;

declare %private function functx:escape-for-regex
  ( $arg as xs:string? )  as xs:string {

   replace($arg,
           '(\.|\[|\]|\\|\||\-|\^|\$|\?|\*|\+|\{|\}|\(|\))','\\$1')
 } ;

(:~
 : List documents in data collection
 :)
declare 
    %templates:wrap
function app:list-works($node as node(), $model as map(*), $filter as xs:string?, $browse as xs:string?) {
    let $cached := session:get-attribute("simple.works")
    let $filtered :=
        if ($filter) then
            let $ordered :=
                for $item in
                    ft:search($config:data-root, $browse || ":" || $filter, ("author", "title"))/search
                let $author := $item/field[@name = "author"]
                order by $author[1], $author[2], $author[3]
                return
                    $item
            for $doc in $ordered
            return
                doc($doc/@uri)/tei:TEI
        else if ($cached and $filter != "") then
            $cached
        else
            collection($config:data-root)/tei:TEI
    return (
        session:set-attribute("simple.works", $filtered),
        session:set-attribute("browse", $browse),
        session:set-attribute("filter", $filter),
        map {
            "all" : $filtered
        }
    )
};

declare
    %templates:wrap
    %templates:default("start", 1)
    %templates:default("per-page", 10)
function app:browse($node as node(), $model as map(*), $start as xs:int, $per-page as xs:int) {
    subsequence($model?all, $start, $per-page) !
        element { node-name($node) } {
            $node/@*,
            templates:process($node/node(), map:new(($model, map { "work": . })))
        }
};

(:~
 : Create a bootstrap pagination element to navigate through the hits.
 :)
declare
    %templates:default('key', 'hits')
    %templates:default('start', 1)
    %templates:default("per-page", 10)
    %templates:default("min-hits", 0)
    %templates:default("max-pages", 10)
function app:paginate($node as node(), $model as map(*), $key as xs:string, $start as xs:int, $per-page as xs:int, $min-hits as xs:int,
    $max-pages as xs:int) {
    if ($min-hits < 0 or count($model($key)) >= $min-hits) then
        element { node-name($node) } {
            $node/@*,
            let $count := xs:integer(ceiling(count($model($key))) div $per-page) + 1
            let $middle := ($max-pages + 1) idiv 2
            return (
                if ($start = 1) then (
                    <li class="disabled">
                        <a><i class="glyphicon glyphicon-fast-backward"/></a>
                    </li>,
                    <li class="disabled">
                        <a><i class="glyphicon glyphicon-backward"/></a>
                    </li>
                ) else (
                    <li>
                        <a href="?start=1"><i class="glyphicon glyphicon-fast-backward"/></a>
                    </li>,
                    <li>
                        <a href="?start={max( ($start - $per-page, 1 ) ) }"><i class="glyphicon glyphicon-backward"/></a>
                    </li>
                ),
                let $startPage := xs:integer(ceiling($start div $per-page))
                let $lowerBound := max(($startPage - ($max-pages idiv 2), 1))
                let $upperBound := min(($lowerBound + $max-pages - 1, $count))
                let $lowerBound := max(($upperBound - $max-pages + 1, 1))
                for $i in $lowerBound to $upperBound
                return
                    if ($i = ceiling($start div $per-page)) then
                        <li class="active"><a href="?start={max( (($i - 1) * $per-page + 1, 1) )}">{$i}</a></li>
                    else
                        <li><a href="?start={max( (($i - 1) * $per-page + 1, 1)) }">{$i}</a></li>,
                if ($start + $per-page < count($model($key))) then (
                    <li>
                        <a href="?start={$start + $per-page}"><i class="glyphicon glyphicon-forward"/></a>
                    </li>,
                    <li>
                        <a href="?start={max( (($count - 1) * $per-page + 1, 1))}"><i class="glyphicon glyphicon-fast-forward"/></a>
                    </li>
                ) else (
                    <li class="disabled">
                        <a><i class="glyphicon glyphicon-forward"/></a>
                    </li>,
                    <li>
                        <a><i class="glyphicon glyphicon-fast-forward"/></a>
                    </li>
                )
            )
        }
    else
        ()
};

(:~
    Create a span with the number of items in the current search result.
:)
declare 
    %templates:wrap
    %templates:default("key", "hits")
function app:hit-count($node as node()*, $model as map(*), $key as xs:string) {
    count($model($key))
};

(:template function in search.html:)
declare function app:query-report($node as node()*, $model as map(*)) {
    let $hits := $model("hits")
    let $hit-count := count($hits)
    let $log := util:log("DEBUG", ("##$hit-count): ", $hit-count))
    let $match-count := count(util:expand($hits)//exist:match)
    let $ids := $model("apps.simple.target-texts")
    let $ids := request:get-parameter('target-texts', 'all')
    let $hit-count-total := session:get-attribute("apps.simple.hitCount")
    let $log := util:log("DEBUG", ("##$hit-count-total): ", $hit-count-total))
    return
        <div xmlns="http://www.w3.org/1999/xhtml" id="query-report"> You have searched for <strong>{$model("query")}</strong> in 
        <strong>{if ($ids = 'all' or empty($ids)) then 'all works' else app:ids-to-titles($ids)}</strong> 
        and found <strong>{$hit-count}</strong>{if ($hit-count eq 1) then ' hit' else ' hits'} with <strong>{$match-count}</strong> {if ($match-count eq 1) then ' match.' else ' matches.'}
        {if ($hit-count-total > $config:hit-limit) then <warning>Your search resulted in {$hit-count-total} hits. Only the first {$config:hit-limit} hits are returned.</warning> else ()}
        </div>
};

declare function app:ids-to-titles($ids as xs:string+) {
    let $titles :=
        for $id in $ids
        return
            collection($config:data-root)//tei:TEI[@xml:id eq $id]/tei:teiHeader/tei:fileDesc/tei:sourceDesc/tei:biblFull/tei:titleStmt/tei:title[1]/text()
    let $count := count($titles)
    return
        app:serialize-list($titles, $count)
};

declare function app:serialize-list($sequence as item()+, $sequence-count as xs:integer) as xs:string {       
    if ($sequence-count eq 1)
        then $sequence
        else
            if ($sequence-count eq 2)
            then concat(
                subsequence($sequence, 1, $sequence-count - 1),
                (:Places " and " before last item.:)
                ' and ',
                $sequence[$sequence-count]
                )
            else concat(
                (:Places ", " after all items that do not come last.:)
                string-join(subsequence($sequence, 1, $sequence-count - 1)
                , ', ')
                ,
                (:Places ", and " before item that comes last.:)
                ', and ',
                $sequence[$sequence-count]
                )
};

(:template function in index.html:)
declare 
    %templates:wrap
function app:checkbox($node as node(), $model as map(*), $target-texts as xs:string*) {
    attribute { "value" } {
        $model("work")/@xml:id/string()
    },
    if ($model("work")/@xml:id/string() = $target-texts) then
        attribute checked { "checked" }
    else
        ()
};


(:~
 : 
 :)
declare function app:work-title($node as node(), $model as map(*), $type as xs:string?) {
    let $suffix := if ($type) then "." || $type else ()
    let $work := $model("work")/ancestor-or-self::tei:TEI
    let $id := util:document-name($work)
    return
        <a xmlns="http://www.w3.org/1999/xhtml" href="{$node/@href}{$id}{$suffix}">{ app:work-title($work) }</a>
};

declare %private function app:work-title($work as element(tei:TEI)?) {
    let $main-title := $work/tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:title[@type = 'main']/text()
    let $main-title := if ($main-title) then $main-title else $work/tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:title[1]/text()
    return
        $main-title
};

declare function app:work-author($node as node(), $model as map(*)) {
    let $work := $model("work")/ancestor-or-self::tei:TEI
    let $work-authors := $work/tei:teiHeader/tei:fileDesc/tei:sourceDesc/tei:biblFull/tei:titleStmt/tei:author
    return 
        string-join($work-authors, "; ")
};

declare 
    %templates:wrap
function app:work-edition($node as node(), $model as map(*)) {
    $model("work")/ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:editionStmt/tei:edition/tei:date/text()
};

declare function app:download-link($node as node(), $model as map(*), $type as xs:string, $doc as xs:string?,
    $source as xs:boolean?) {
    let $file := 
        if ($model?work) then 
            replace(util:document-name($model("work")), "^(.*?)\.[^\.]*$", "$1")
        else 
            replace($doc, "^(.*)\..*$", "$1")
    let $uuid := util:uuid()
    return
        element { node-name($node) } {
            $node/@*,
            attribute data-token { $uuid },
            attribute href { $node/@href || $file || "." || $type || "?token=" || $uuid || "&amp;cache=no" 
                || (if ($source) then "&amp;source=yes" else ())
            },
            $node/node()
        }
};

declare
    %templates:wrap
function app:fix-links($node as node(), $model as map(*)) {
    app:fix-links(templates:process($node/node(), $model))
};

declare function app:fix-links($nodes as node()*) {
    for $node in $nodes
    return
        typeswitch($node)
            case element(a) | element(link) return
                (: skip links with @data-template attributes; otherwise we can run into duplicate @href errors :)
                if ($node/@data-template) then
                    $node
                else
                    let $href :=
                        replace(
                            $node/@href,
                            "\$app",
                            (request:get-context-path() || substring-after($config:app-root, "/db"))
                        )
                    return
                        element { node-name($node) } {
                            attribute href {$href}, $node/@* except $node/@href, app:fix-links($node/node())
                        }
            case element() return
                element { node-name($node) } {
                    $node/@*, app:fix-links($node/node())
                }
            default return
                $node
};

(: Search :)

(:~
: Execute the query. The search results are not output immediately. Instead they
: are passed to nested templates through the $model parameter.
:
: @author Wolfgang M. Meier
: @author Jens Østergaard Petersen
: @param $node 
: @param $model
: @param $query The query string. 
: @param $index The index against which the query is to be performed, as the string "ngram" or "lucene".
: @param $tei-target A sequence of one or more targets within a TEI document, the tei:teiHeader or tei:text.
: @param $work-authors A sequence of the string "all" or of the xml:ids of the documents associated with the selected authors.
: @param $target-texts A sequence of the string "all" or of the xml:ids of the documents selected.

: @return The function returns a map containing the $hits, the $query, and the $query-scope. The search results are output through the nested templates, app:hit-count, app:paginate, and app:show-hits.
:)
declare 
    %templates:default("tei-target", "tei-text")
    %templates:default("query-scope", "p")
    %templates:default("work-authors", "all")
    %templates:default("target-texts", "all")
    %templates:default("bool", "new")
function app:query($node as node()*, $model as map(*), $query as xs:string?, $tei-target as xs:string+, $query-scope as xs:string, $work-authors as xs:string+, $target-texts as xs:string+, $bool as xs:string?) as map(*) {
        let $query := 
            if ($query) 
            then app:convert-query-to-phrase-query(app:sanitize-query($query)) 
            else ()
        return
        (:If there is no query string, fill up the map with any existing values (but do not sanitize it once again):)
        if (empty($query))
        then
            map {
                "hits" := session:get-attribute("apps.simple.hits"),
                "hitCount" := session:get-attribute("apps.simple.hitCount"),
                "query" := session:get-attribute("apps.simple.query"),
                "scope" := session:get-attribute("apps.simple.scope"),
                "target-texts" := session:get-attribute("apps.simple.target-texts"),
                "bool" := session:get-attribute("apps.simple.bool")
            }
        else
            (:Otherwise, perform the query.:)
            (: Here the actual query commences. This is split into two parts, the first for a Lucene query and the second for an ngram query. :)
            let $hits :=
                (:If the $query-scope is narrow, query the elements immediately below the lowest div in tei:text and the four major element below tei:teiHeader.:)
                if ($query-scope eq 'seg')
                then
                    for $hit in 
                        (:If both tei-text and tei-header is queried.:)
                        if (count($tei-target) eq 2)
                        then
                            collection($config:data-root)//tei:seg[ft:query(., $query)] |
                            collection($config:data-root)//tei:teiHeader[ft:query(., $query)]
                        else
                            if ($tei-target = 'tei-text')
                            then
                                collection($config:data-root)//tei:seg[ft:query(., $query)]
                            else 
                                if ($tei-target = 'tei-head')
                                then 
                                    collection($config:data-root)//tei:teiHeader[ft:query(., $query)]
                                else ()
                    order by ft:score($hit) descending
                    return $hit
                else 
                if ($query-scope eq 'p')
                then
                    for $hit in 
                        (:If both tei-text and tei-header is queried.:)
                        if (count($tei-target) eq 2)
                        then
                            collection($config:data-root)//tei:p[ft:query(., $query)] |
                            collection($config:data-root)//tei:teiHeader[ft:query(., $query)]
                        else
                            if ($tei-target = 'tei-text')
                            then
                                collection($config:data-root)//tei:p[ft:query(., $query)]
                            else 
                                if ($tei-target = 'tei-head')
                                then 
                                    collection($config:data-root)//tei:teiHeader[ft:query(., $query)]
                                else ()
                    order by ft:score($hit) descending
                    return $hit
                else 
                if ($query-scope eq 'div')
                then
                    for $hit in 
                        (:If both tei-text and tei-header is queried.:)
                        if (count($tei-target) eq 2)
                        then
                            collection($config:data-root)//tei:div[ft:query(., $query)] |
                            collection($config:data-root)//tei:teiHeader[ft:query(., $query)]
                        else
                            if ($tei-target = 'tei-text')
                            then
                                collection($config:data-root)//tei:div[ft:query(., $query)]
                            else 
                                if ($tei-target = 'tei-head')
                                then 
                                    collection($config:data-root)//tei:teiHeader[ft:query(., $query)]
                                else ()

                    order by ft:score($hit) descending
                    return $hit
                else ()
            let $query :=
                        if ($bool eq 'new')
                        then $query
                        else
                            if ($bool eq 'and')
                            then session:get-attribute("apps.simple.query") || ' AND ' || $query
                            else
                                if ($bool eq 'or')
                                then session:get-attribute("apps.simple.query") || ' OR ' || $query
                                else
                                    if ($bool eq 'not')
                                    then session:get-attribute("apps.simple.query") || ' NOT ' || $query
                                    else $query
            let $hits :=
                if ($bool eq 'or')
                then session:get-attribute("apps.simple.hits") union $hits
                else 
                    if ($bool eq 'and')
                    then session:get-attribute("apps.simple.hits") intersect $hits
                    else
                        if ($bool eq 'not')
                        then session:get-attribute("apps.simple.hits") except $hits
                        else $hits
            (:Store the result in the session.:)
            let $hitCount := count($hits)
            let $hits := if ($hitCount > 1000) then subsequence($hits, 1, 1000) else $hits
            (:Store the result in the session.:)
            let $store := (
                session:set-attribute("apps.simple.hits", $hits),
                session:set-attribute("apps.simple.hitCount", $hitCount),
                session:set-attribute("apps.simple.query", $query),
                session:set-attribute("apps.simple.scope", $query-scope),
                session:set-attribute("apps.simple.target-texts", $target-texts),
                session:set-attribute("apps.simple.bool", $bool)
                )
            return
                (: The hits are not returned directly, but processed by the nested templates :)
                map {
                    "hits" := $hits,
                    "hitCount" := $hitCount,
                    "query" := $query
                }
};


(:convert text queries into phrase queries by default:)
declare function app:convert-query-to-phrase-query($query as xs:string) as xs:string {
    let $query-parts := tokenize($query, '\s+')
    return
        string-join(
        for $query-part in $query-parts
        return 
            if (
                (
                    $query-part = ('AND', 'OR', 'NOT') or 
                    (:if the first character, including any + or - or ' or ", is not a Chinese character, don't alter the query, assuming that the rest will be the same:)
                    (:if users employ +, -, ' or ", they must know what they are doing:)
                    string-to-codepoints(substring($query-part, 1)) < 13312
                )
                (:if the length of the query part, including any + or - or ' or ", is greater than 1, don't alter the query:)
                and string-length($query-part) > 1
                )
            then $query-part
            else '"' || app:sanitize-query-part($query-part) || '"'
        , ' ')
};

declare function app:sanitize-query-part($query as xs:string?) as xs:string? {
    if (functx:contains-any-of($query, ('{', '}', '[', ']')))
        then 
            (:in a regex query, {}[] are OK:)
            if (starts-with($query, '/') and ends-with($query, '/'))
            then $query
            (:otherwise remove them:)
            else translate($query, '{}[]', ' ')
        else $query
};

(: This functions provides crude way to avoid the most common errors with paired expressions and apostrophes. :)
(: TODO: check order of pairs:)
declare %private function app:sanitize-query($query-string as xs:string) as xs:string {
    let $query-string := replace($query-string, "'", "''") (:escape apostrophes:)
    (:TODO: notify user if query has been modified.:)
    (:Remove colons – Lucene fields are not supported.:)
    let $query-string := translate($query-string, ":", " ")
    let $query-string := 
       if (functx:number-of-matches($query-string, '"') mod 2) 
       then $query-string
       else replace($query-string, '"', ' ') (:if there is an uneven number of quotation marks, delete all quotation marks.:)
    let $query-string := 
       if ((functx:number-of-matches($query-string, '\(') + functx:number-of-matches($query-string, '\)')) mod 2 eq 0) 
       then $query-string
       else translate($query-string, '()', ' ') (:if there is an uneven number of parentheses, delete all parentheses.:)
    let $query-string := 
       if ((functx:number-of-matches($query-string, '\[') + functx:number-of-matches($query-string, '\]')) mod 2 eq 0) 
       then $query-string
       else translate($query-string, '[]', ' ') (:if there is an uneven number of brackets, delete all brackets.:)
    let $query-string := 
       if ((functx:number-of-matches($query-string, '{') + functx:number-of-matches($query-string, '}')) mod 2 eq 0) 
       then $query-string
       else translate($query-string, '{}', ' ') (:if there is an uneven number of braces, delete all braces.:)
    let $query-string := 
       if ((functx:number-of-matches($query-string, '<') + functx:number-of-matches($query-string, '>')) mod 2 eq 0) 
       then $query-string
       else translate($query-string, '<>', ' ') (:if there is an uneven number of angle brackets, delete all angle brackets.:)
    return $query-string
};
(:~
    Output the actual search result as a div, using the kwic module to summarize full text matches.
:)
declare 
    %templates:wrap
    %templates:default("start", 1)
    %templates:default("per-page", 10)
function app:show-hits($node as node()*, $model as map(*), $start as xs:integer, $per-page as xs:integer) {
    for $hit at $p in subsequence($model("hits"), $start, $per-page)
    let $parent := $hit/ancestor-or-self::tei:div[1]
    let $parent := if ($parent) then $parent else $hit/ancestor-or-self::tei:teiHeader  
    let $div := app:get-current($parent)
    let $parent-id := util:document-name($parent) || "_" || util:node-id($parent)
    let $div-id := util:document-name($div) || "_" || util:node-id($div)
    (:if the nearest div does not have an xml:id, find the nearest element with an xml:id and use it:)
    (:is this necessary - can't we just use the nearest ancestor?:) 
(:    let $div-id := :)
(:        if ($div-id) :)
(:        then $div-id :)
(:        else ($hit/ancestor-or-self::*[@xml:id]/@xml:id)[1]/string():)
    (:if it is not a div, it will not have a head:)
    let $div-head := $parent/tei:head/text()
    (:TODO: what if the hit is in the header?:)
    let $work := $hit/ancestor::tei:TEI
    let $work-title := app:work-title($work)
    (:the work always has xml:id.:)
    let $work-id := $work/@xml:id/string()
    let $work-id := if ($work-id) then $work-id else util:document-name($work) || "_1"
    (:pad hit with surrounding siblings:)
    let $hit-padded := $hit
(:    let $hit-padded := <hit>{($hit/preceding-sibling::*[1], $hit, $hit/following-sibling::*[1])}</hit>:)
    let $loc := 
        <tr class="reference">
            <td colspan="3">
                <span class="number">{$start + $p - 1}</span>
                <a href="{$work-id}">{$work-title}</a>{if ($div-head) then ' / ' else ''}<a href="{$parent-id}.html?action=search">{$div-head}</a>
            </td>
        </tr>
    let $matchId := util:node-id($hit)
    let $config := <config width="60" table="yes" link="{$div-id}.xml?action=search#{$matchId}"/>
    let $expanded := util:expand($hit)
    return (
        $loc,
        for $match in subsequence($expanded//exist:match, 1, 5)
        let $kwic := kwic:get-summary($expanded, $match, $config)
        return $kwic
    )
};

declare %private function app:get-current($div as element()?) {
    if (empty($div)) then
        ()
    else
        if ($div instance of element(tei:teiHeader)) then
        $div
        else
            if (
                empty($div/preceding-sibling::tei:div)  (: first div in section :)
                and count($div/preceding-sibling::*) < 5 (: less than 5 elements before div :)
                and $div/.. instance of element(tei:div) (: parent is a div :)
            ) then
                pages:get-previous($div/..)
            else
                $div
};