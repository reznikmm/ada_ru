define(['vs/editor/editor.main'], function() {
    monaco.languages.register({
        id: 'ada'
    });

    var modelUri = monaco.Uri.parse("file://tmp/_source.adb");
    var model = monaco.editor.createModel("", "ada", modelUri);
    model.updateOptions({
        tabSize: 3
    });

    config = {
        comments: {
            lineComment: '--',
        },
        surroundingPairs: [{
                open: '(',
                close: ')'
            },
            {
                open: '"',
                close: '"'
            },
            {
                open: "'",
                close: "'"
            }
        ],
        autoClosingPairs: [{
                open: '(',
                close: ')'
            },
            {
                open: '"',
                close: '"'
            },
            {
                open: "'",
                close: "'"
            }
        ],
        brackets: [
            ['(', ')'],
        ],
        onEnterRules: [{
                beforeText: /^\s*(?:--.*)?$/,
                action: {
                    indentAction: monaco.languages.IndentAction.None
                }
            },
            {
                beforeText: /;\s*(?:--.*)?$/,
                action: {
                    indentAction: monaco.languages.IndentAction.None
                }
            },
            {
                beforeText: /\b(?:is|record|then|loop|do|select)\s*(?:--.*)?$/,
                action: {
                    indentAction: monaco.languages.IndentAction.Indent
                }
            },
            {
                beforeText: /\bend\b/,
                action: {
                    indentAction: monaco.languages.IndentAction.Outdent,
                    outdentCurrentLine: true
                }
            },
            {
                beforeText: /^\s*(?:begin|private|else|exception|or)*(?:--.*)?$/,
                action: {
                    indentAction: monaco.languages.IndentAction.Indent,
                    outdentCurrentLine: true
                }
            },
            {
                beforeText: /./,
                action: {
                    indentAction: monaco.languages.IndentAction.Indent,
                    removeText: 1
                }
            },
        ],
    }
    monaco.languages.setLanguageConfiguration("ada", config)
    monaco.languages.setMonarchTokensProvider('ada', {
        ignoreCase: true,
        // Set defaultToken to invalid to see what you do not tokenize yet
        //defaultToken: 'invalid',
        brackets: [
            ['(', ')', 'delimiter.parenthesis']
        ],

        keywords: [
            'abort', 'abs', 'abstract', 'accept', 'access', 'aliased', 'all', 'and', 'array',
            'at', 'begin', 'body', 'case', 'constant', 'declare', 'delay', 'delta', 'digits',
            'do', 'else', 'elsif', 'end', 'entry', 'exception', 'exit', 'for', 'function',
            'generic', 'goto', 'if', 'in', 'interface', 'is', 'limited', 'loop', 'mod',
            'new', 'not', 'null', 'of', 'or', 'others', 'out', 'overriding', 'package',
            'pragma', 'private', 'procedure', 'protected', 'raise', 'range', 'record',
            'rem', 'renames', 'requeue', 'return', 'reverse', 'select', 'separate', 'some',
            'subtype', 'synchronized', 'tagged', 'task', 'terminate', 'then', 'type',
            'until', 'use', 'when', 'while', 'with', 'xor'
        ],

        operators: [
            '=>', '..', '**', ':=', '/=', '>=', '<=', '<<', '>>', '<>',
            '&', '*', '+', '-', '/', '<', '=', '>', '|'
        ],

        identifier_start: '[a-zA-Z]',
        identifier_extend: '[0-9]',
        pc: '[_\u203F\u2040\u2054\uFE33\uFE34\uFE4D-\uFE4F\uFF3F]',

        numeral: '[0-9](?:_?[0-9])*',
        exponent: '[Ee][-+]?@numeral',
        decimal_literal: '@numeral(?:[.]@numeral)?@exponent?',
        extended_digit: '[0-9a-fA-F]',
        based_numeral: '@extended_digit(?:_?@extended_digit)*',
        based_literal: '@numeral#@based_numeral(?:\.@based_numeral)?#@exponent?',
        numeric_literal: '(?:@based_literal|@decimal_literal)',

        non_quotation: '[^\"\\s]',
        string_element: '@non_quotation|\"\"',

        // The main tokenizer for our languages
        tokenizer: {
            root: [
                // identifiers and keywords
                ['@identifier_start(?:@pc?(?:@identifier_start|@identifier_extend))*',
                    {
                        cases: {
                            '@keywords': 'keyword',
                            '@default': 'identifier'
                        }
                    }
                ],

                ['@numeric_literal', 'number'],
                ['\'.\'', 'string'],
                ['"@string_element*"', 'string'],
                ['--.*$', 'comment'],
                ['[()]', '@brackets'],
                ['[&\'\\*\\+,\\-./:;<=>|]+', {
                    cases: {
                        '@operators': 'operator',
                        '@default': ''
                    }
                }],
            ],

        },
    });

    var editor = monaco.editor.create(document.getElementById('container'), {
        model: model,
        language: 'ada'
    });

    function init(slug){
        var host = `${document.location.protocol}//${document.location.host}`;
        var initial = `${host}/game/${slug}/${slug}.txt`;

        fetch(`${host}/game/code/${slug}`)
          .then (response => response.ok?response.text():
            fetch(initial)
              .then (response => response.ok?response.text():''))
          .then (text => model.setValue (text));

          document.getElementById('reset_button').onclick = function() {
            fetch(initial)
              .then (response => response.ok?response.text():'')
              .then (text => model.setValue (text));
        };

        document.getElementById('help_button').onclick = function() {
            editor.focus();
            editor.getAction('editor.action.quickCommand').run();
        };
    };

    return init;
});
