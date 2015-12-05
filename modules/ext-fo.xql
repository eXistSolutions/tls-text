xquery version "3.1";

(:~
 : Non-standard extension functions, mainly used for the documentation.
 :)
module namespace pmf="http://existsolutions.com/apps/TLS/texts/ext-fo";

import module namespace print="http://www.tei-c.org/tei-simple/xquery/functions/fo" at "/db/apps/tei-simple/content/fo-functions.xql";

declare namespace tei="http://www.tei-c.org/ns/1.0";
declare namespace fo="http://www.w3.org/1999/XSL/Format";

declare function pmf:alternate($config as map(*), $node as element(), $class as xs:string+, $content, $default as node()*,
    $alternate as node()*) {
    let $number := counter:next-value($print:NOTE_COUNTER_ID)
    return (
        print:inline($config, $node, $class, $default),
        <fo:footnote>
            <fo:inline>
            {print:check-styles($config, $node, "note", ())}
            {$number} 
            </fo:inline>
            <fo:footnote-body start-indent="0mm" end-indent="0mm" text-indent="0mm" white-space-treatment="ignore-if-surrounding-linefeed">
                <fo:list-block>
                    <fo:list-item>
                        <fo:list-item-label end-indent="label-end()" >
                            <fo:block>
                            {print:check-styles($config, (), "note-body-number", ())}
                            { $number }
                            </fo:block>
                        </fo:list-item-label>
                        <fo:list-item-body start-indent="body-start()">
                            {print:check-styles($config, (), "note-body", ())}
                            <fo:block>{$config?apply-children($config, $node, $alternate)}</fo:block>
                        </fo:list-item-body>
                    </fo:list-item>
                </fo:list-block>
            </fo:footnote-body>
        </fo:footnote>
    )
};