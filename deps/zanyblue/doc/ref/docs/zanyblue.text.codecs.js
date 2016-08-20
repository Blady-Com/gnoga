GNATdoc.Documentation = {
  "label": "ZanyBlue.Text.Codecs",
  "summary": [
  ],
  "description": [
  ],
  "entities": [
    {
      "entities": [
        {
          "label": "Unsupported_Encoding_Action_Type",
          "line": 70,
          "column": 9,
          "src": "srcs/zanyblue-text-codecs.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 70,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "   "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "type"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Unsupported_Encoding_Action_Type",
                      "href": "docs/zanyblue.text.codecs.html#L70C9"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "is"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Use_UTF8",
                      "href": "docs/zanyblue.text.codecs.html#L70C46"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ","
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Raise_Exception",
                      "href": "docs/zanyblue.text.codecs.html#L70C56"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ")"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";",
                      "href": "docs/zanyblue.text.codecs.html#L70C9"
                    }
                  ]
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "How to handle user selection of an unsupported encoding.  The\n"
                },
                {
                  "kind": "span",
                  "text": "default is to simply use \"UTF-8\" for all unsupported encodings.\n"
                }
              ]
            }
          ],
          "literals": [
            {
              "label": "Use_UTF8",
              "line": 70,
              "column": 46,
              "description": [
              ]
            },
            {
              "label": "Raise_Exception",
              "line": 70,
              "column": 56,
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Unicode_XForm_Action_Type",
          "line": 74,
          "column": 9,
          "src": "srcs/zanyblue-text-codecs.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 74,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "   "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "type"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Unicode_XForm_Action_Type",
                      "href": "docs/zanyblue.text.codecs.html#L74C9"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "is"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Replace",
                      "href": "docs/zanyblue.text.codecs.html#L74C39"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ","
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Raise_Exception",
                      "href": "docs/zanyblue.text.codecs.html#L74C48"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ")"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";",
                      "href": "docs/zanyblue.text.codecs.html#L74C9"
                    }
                  ]
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "How to handle encoding or decoding errors, e.g., attempting to\n"
                },
                {
                  "kind": "span",
                  "text": "encoding an Unicode character not supported by the targe encoding.\n"
                },
                {
                  "kind": "span",
                  "text": "The default is to simply replace the character with the ASCII character\n"
                },
                {
                  "kind": "span",
                  "text": "'?'.\n"
                }
              ]
            }
          ],
          "literals": [
            {
              "label": "Replace",
              "line": 74,
              "column": 39,
              "description": [
              ]
            },
            {
              "label": "Raise_Exception",
              "line": 74,
              "column": 48,
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Implementation_Type",
          "line": 80,
          "column": 9,
          "src": "srcs/zanyblue-text-codecs.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 80,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "   "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "type"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Implementation_Type",
                      "href": "docs/zanyblue.text.codecs.html#L80C9"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "is"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Internal",
                      "href": "docs/zanyblue.text.codecs.html#L80C33"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ","
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Data_Driven",
                      "href": "docs/zanyblue.text.codecs.html#L80C43"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ")"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";",
                      "href": "docs/zanyblue.text.codecs.html#L80C9"
                    }
                  ]
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "The internal ZanyBlue implementation of the encoding: either internal,\n"
                },
                {
                  "kind": "span",
                  "text": "i.e., directly implemented via internal routines, or data driven, i.e.,\n"
                },
                {
                  "kind": "span",
                  "text": "via code point mapping tables.\n"
                }
              ]
            }
          ],
          "literals": [
            {
              "label": "Internal",
              "line": 80,
              "column": 33,
              "description": [
              ]
            },
            {
              "label": "Data_Driven",
              "line": 80,
              "column": 43,
              "description": [
              ]
            }
          ]
        }
      ],
      "label": "Simple types"
    },
    {
      "entities": [
        {
          "label": "Codecs_Type",
          "line": 43,
          "column": 9,
          "src": "srcs/zanyblue-text-codecs.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 43,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "   "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "type"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Codecs_Type",
                      "href": "docs/zanyblue.text.codecs.html#L43C9"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "is"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "tagged"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "private"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";"
                    }
                  ]
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "Representation of a codec, encoding/decoding to/from an external\n"
                },
                {
                  "kind": "span",
                  "text": "encoding, e.g., \"UTF-8\", \"ISO8859-1\", etc.\n"
                }
              ]
            }
          ]
        }
      ],
      "label": "Tagged types"
    },
    {
      "entities": [
        {
          "label": "Set_Unsupported_Encoding_Action",
          "line": 85,
          "column": 14,
          "src": "srcs/zanyblue-text-codecs.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 85,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "   "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "procedure"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Set_Unsupported_Encoding_Action",
                      "href": "docs/zanyblue.text.codecs.html#L85C14"
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 86,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "      "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Action",
                      "href": "docs/zanyblue.text.codecs.html#L86C8"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ":"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Unsupported_Encoding_Action_Type",
                      "href": "docs/zanyblue.text.codecs.html#L70C9"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ")"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";"
                    }
                  ]
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "Action to take when an unsupported Encoding is selected: either\n"
                },
                {
                  "kind": "span",
                  "text": "fallback to UTF-8 (the default action) or raise the exception\n"
                },
                {
                  "kind": "span",
                  "text": "\"Unsupported_Encoding\".\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Action",
              "line": 86,
              "column": 8,
              "type": {
                "label": "ZanyBlue.Text.Codecs.Unsupported_Encoding_Action_Type",
                "docHref": "docs/zanyblue.text.codecs.html#L70C9"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Set_Unicode_Encode_Action",
          "line": 91,
          "column": 14,
          "src": "srcs/zanyblue-text-codecs.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 91,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "   "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "procedure"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Set_Unicode_Encode_Action",
                      "href": "docs/zanyblue.text.codecs.html#L91C14"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Action",
                      "href": "docs/zanyblue.text.codecs.html#L91C41"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ":"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Unicode_XForm_Action_Type",
                      "href": "docs/zanyblue.text.codecs.html#L74C9"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ")"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";"
                    }
                  ]
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "Action to take when encoding from Unicode to the target encoding\n"
                },
                {
                  "kind": "span",
                  "text": "fails.  The default action is to simply replace the character with\n"
                },
                {
                  "kind": "span",
                  "text": "the ASCII character '?\" (an \"Action\" value of \"Replace\").\n"
                },
                {
                  "kind": "span",
                  "text": "Alternatively, the exception \"Unicode_Encode_Error\" can be raised\n"
                },
                {
                  "kind": "span",
                  "text": "(an \"Action\" value of \"Raise_Exception\").\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Action",
              "line": 91,
              "column": 41,
              "type": {
                "label": "ZanyBlue.Text.Codecs.Unicode_XForm_Action_Type",
                "docHref": "docs/zanyblue.text.codecs.html#L74C9"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Set_Unicode_Decode_Action",
          "line": 98,
          "column": 14,
          "src": "srcs/zanyblue-text-codecs.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 98,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "   "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "procedure"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Set_Unicode_Decode_Action",
                      "href": "docs/zanyblue.text.codecs.html#L98C14"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Action",
                      "href": "docs/zanyblue.text.codecs.html#L98C41"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ":"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Unicode_XForm_Action_Type",
                      "href": "docs/zanyblue.text.codecs.html#L74C9"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ")"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";"
                    }
                  ]
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "Action to take when de`oding from a target encoded string to Unicode\n"
                },
                {
                  "kind": "span",
                  "text": "fails.  The default action is to simply replace the character(s) with\n"
                },
                {
                  "kind": "span",
                  "text": "the ASCII character '?\" (an \"Action\" value of \"Replace\").\n"
                },
                {
                  "kind": "span",
                  "text": "Alternatively, the exception \"Unicode_Encode_Error\" can be raised\n"
                },
                {
                  "kind": "span",
                  "text": "(an \"Action\" value of \"Raise_Exception\").\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Action",
              "line": 98,
              "column": 41,
              "type": {
                "label": "ZanyBlue.Text.Codecs.Unicode_XForm_Action_Type",
                "docHref": "docs/zanyblue.text.codecs.html#L74C9"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Encode",
          "line": 137,
          "column": 13,
          "src": "srcs/zanyblue-text-codecs.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 137,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "   "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "function"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Encode",
                      "href": "docs/zanyblue.text.codecs.html#L137C13"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Encoding",
                      "href": "docs/zanyblue.text.codecs.html#L137C21"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ":"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Wide_String"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";"
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 138,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "                    "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Value",
                      "href": "docs/zanyblue.text.codecs.html#L138C21"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ":"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Wide_String"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ")"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "return"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "String"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";"
                    }
                  ]
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "Encode the Unicode string using the named encoding.  This could raise\n"
                },
                {
                  "kind": "span",
                  "text": "the exception \"Unicode_Encode_Error\" if requested via the routine\n"
                },
                {
                  "kind": "span",
                  "text": "\"Set_Unicode_Encode_Action\".  The default action is to simply replace\n"
                },
                {
                  "kind": "span",
                  "text": "un-encodeable character with the ASCII character '?'.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Encoding",
              "line": 137,
              "column": 21,
              "type": {
                "label": "Wide_String"
              },
              "description": [
              ]
            },
            {
              "label": "Value",
              "line": 138,
              "column": 21,
              "type": {
                "label": "Wide_String"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Decode",
          "line": 144,
          "column": 13,
          "src": "srcs/zanyblue-text-codecs.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 144,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "   "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "function"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Decode",
                      "href": "docs/zanyblue.text.codecs.html#L144C13"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Encoding",
                      "href": "docs/zanyblue.text.codecs.html#L144C21"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ":"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Wide_String"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";"
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 145,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "                    "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Value",
                      "href": "docs/zanyblue.text.codecs.html#L145C21"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ":"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "String"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ")"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "return"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Wide_String"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";"
                    }
                  ]
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "Decode the encoded argument string using the named encoding.  The\n"
                },
                {
                  "kind": "span",
                  "text": "exception \"Unicode_Decode_Error\" can be raised depending on the setting\n"
                },
                {
                  "kind": "span",
                  "text": "selected by \"Set_Unicode_Decode_Action\", the default is to simply\n"
                },
                {
                  "kind": "span",
                  "text": "replace the un-decodeable sequence with the ASCII character '?'.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Encoding",
              "line": 144,
              "column": 21,
              "type": {
                "label": "Wide_String"
              },
              "description": [
              ]
            },
            {
              "label": "Value",
              "line": 145,
              "column": 21,
              "type": {
                "label": "String"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "N_Encodings",
          "line": 151,
          "column": 13,
          "src": "srcs/zanyblue-text-codecs.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 151,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "   "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "function"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "N_Encodings",
                      "href": "docs/zanyblue.text.codecs.html#L151C13"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "return"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Natural"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";"
                    }
                  ]
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "Return the number of encodings defined in the ZanyBlue library.\n"
                }
              ]
            }
          ]
        },
        {
          "label": "Name",
          "line": 154,
          "column": 13,
          "src": "srcs/zanyblue-text-codecs.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 154,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "   "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "function"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Name",
                      "href": "docs/zanyblue.text.codecs.html#L154C13"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Encoding_Index",
                      "href": "docs/zanyblue.text.codecs.html#L154C19"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ":"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Positive"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ")"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "return"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Wide_String"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";"
                    }
                  ]
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "Return the name of the I'th encoding.  This routine will raise\n"
                },
                {
                  "kind": "span",
                  "text": "Constraint_Error if there is no corresponding encoding.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Encoding_Index",
              "line": 154,
              "column": 19,
              "type": {
                "label": "Positive"
              },
              "description": [
              ]
            }
          ]
        }
      ],
      "label": "Subprograms"
    },
    {
      "entities": [
        {
          "label": "Make_Codecs",
          "line": 105,
          "column": 13,
          "src": "srcs/zanyblue-text-codecs.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 105,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "   "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "function"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Make_Codecs",
                      "href": "docs/zanyblue.text.codecs.html#L105C13"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Encoding",
                      "href": "docs/zanyblue.text.codecs.html#L105C26"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ":"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Wide_String"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ")"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "return"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Codecs_Type",
                      "href": "docs/zanyblue.text.codecs.html#L176C9"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";"
                    }
                  ]
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "Create a Codecs object given an encoding name.   Depending on the\n"
                },
                {
                  "kind": "span",
                  "text": "\"Action\" selected by \"Set_Unsupported_Encoding_Action\", this routine\n"
                },
                {
                  "kind": "span",
                  "text": "could raise \"Unsupported_Encoding\" for unsupported encodings.  The\n"
                },
                {
                  "kind": "span",
                  "text": "default is to simply fallback on \"UTF-8\" for unsupported encodings.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Encoding",
              "line": 105,
              "column": 26,
              "type": {
                "label": "Wide_String"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Name",
          "line": 111,
          "column": 13,
          "src": "srcs/zanyblue-text-codecs.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 111,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "   "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "function"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Name",
                      "href": "docs/zanyblue.text.codecs.html#L111C13"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Codecs",
                      "href": "docs/zanyblue.text.codecs.html#L111C19"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ":"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Codecs_Type",
                      "href": "docs/zanyblue.text.codecs.html#L176C9"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ")"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "return"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Wide_String"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";"
                    }
                  ]
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "Return the encoding name associated with a Codecs objects.  If the\n"
                },
                {
                  "kind": "span",
                  "text": "\"Action\" selected by \"Set_Unsupported_Encoding_Action\" is \"Use_UTF8\",\n"
                },
                {
                  "kind": "span",
                  "text": "(the default) this routine might return a name different from the\n"
                },
                {
                  "kind": "span",
                  "text": "string used to create the Codecs objects.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Codecs",
              "line": 111,
              "column": 19,
              "type": {
                "label": "ZanyBlue.Text.Codecs.Codecs_Type",
                "docHref": "docs/zanyblue.text.codecs.html#L43C9"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Implementation",
          "line": 117,
          "column": 13,
          "src": "srcs/zanyblue-text-codecs.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 117,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "   "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "function"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Implementation",
                      "href": "docs/zanyblue.text.codecs.html#L117C13"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Codecs",
                      "href": "docs/zanyblue.text.codecs.html#L117C29"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ":"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Codecs_Type",
                      "href": "docs/zanyblue.text.codecs.html#L176C9"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ")"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "return"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Implementation_Type",
                      "href": "docs/zanyblue.text.codecs.html#L80C9"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";"
                    }
                  ]
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "How is the codecs implemented, is it internally implemented or via\n"
                },
                {
                  "kind": "span",
                  "text": "code point data tables.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Codecs",
              "line": 117,
              "column": 29,
              "type": {
                "label": "ZanyBlue.Text.Codecs.Codecs_Type",
                "docHref": "docs/zanyblue.text.codecs.html#L43C9"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Encode",
          "line": 121,
          "column": 13,
          "src": "srcs/zanyblue-text-codecs.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 121,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "   "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "function"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Encode",
                      "href": "docs/zanyblue.text.codecs.html#L121C13"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Codecs",
                      "href": "docs/zanyblue.text.codecs.html#L121C21"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ":"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Codecs_Type",
                      "href": "docs/zanyblue.text.codecs.html#L176C9"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";"
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 122,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "                    "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Value",
                      "href": "docs/zanyblue.text.codecs.html#L122C21"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ":"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Wide_String"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ")"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "return"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "String"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";"
                    }
                  ]
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "Encode the Unicode string using the encoding associated with the\n"
                },
                {
                  "kind": "span",
                  "text": "Codecs objects.  This could raise the exception \"Unicode_Encode_Error\"\n"
                },
                {
                  "kind": "span",
                  "text": "if requested via the routine \"Set_Unicode_Encode_Action\".  The default\n"
                },
                {
                  "kind": "span",
                  "text": "action is to simply replace un-encodeable character with the ASCII\n"
                },
                {
                  "kind": "span",
                  "text": "character '?'.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Codecs",
              "line": 121,
              "column": 21,
              "type": {
                "label": "ZanyBlue.Text.Codecs.Codecs_Type",
                "docHref": "docs/zanyblue.text.codecs.html#L43C9"
              },
              "description": [
              ]
            },
            {
              "label": "Value",
              "line": 122,
              "column": 21,
              "type": {
                "label": "Wide_String"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Decode",
          "line": 129,
          "column": 13,
          "src": "srcs/zanyblue-text-codecs.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 129,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "   "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "function"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Decode",
                      "href": "docs/zanyblue.text.codecs.html#L129C13"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Codecs",
                      "href": "docs/zanyblue.text.codecs.html#L129C21"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ":"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Codecs_Type",
                      "href": "docs/zanyblue.text.codecs.html#L176C9"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";"
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 130,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "                    "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Value",
                      "href": "docs/zanyblue.text.codecs.html#L130C21"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ":"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "String"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ")"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "return"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Wide_String"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";"
                    }
                  ]
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "Decode the encoded argument string using the encoding associated with\n"
                },
                {
                  "kind": "span",
                  "text": "the Codecs object.  The exception \"Unicode_Decode_Error\" can be raised\n"
                },
                {
                  "kind": "span",
                  "text": "depending on the setting selected by \"Set_Unicode_Decode_Action\", the\n"
                },
                {
                  "kind": "span",
                  "text": "default is to simply replace the un-decodeable sequence with the ASCII\n"
                },
                {
                  "kind": "span",
                  "text": "character '?'.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Codecs",
              "line": 129,
              "column": 21,
              "type": {
                "label": "ZanyBlue.Text.Codecs.Codecs_Type",
                "docHref": "docs/zanyblue.text.codecs.html#L43C9"
              },
              "description": [
              ]
            },
            {
              "label": "Value",
              "line": 130,
              "column": 21,
              "type": {
                "label": "String"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Iterate_Decodings",
          "line": 158,
          "column": 14,
          "src": "srcs/zanyblue-text-codecs.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 158,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "   "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "procedure"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Iterate_Decodings",
                      "href": "docs/zanyblue.text.codecs.html#L158C14"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 159,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "      "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Codecs",
                      "href": "docs/zanyblue.text.codecs.html#L159C7"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "   "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ":"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Codecs_Type",
                      "href": "docs/zanyblue.text.codecs.html#L176C9"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";"
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 160,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "      "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Handler",
                      "href": "docs/zanyblue.text.codecs.html#L160C7"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ":"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "not"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "null"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "access"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "procedure"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "WCh",
                      "href": "docs/zanyblue.text.codecs.html#L160C44"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ":"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Wide_Character"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";"
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 161,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "                                           "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Encoding",
                      "href": "docs/zanyblue.text.codecs.html#L161C44"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ":"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "String"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ")"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ")"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";"
                    }
                  ]
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "Iterate over the encoding mappings for a Codecs.  The iteration is\n"
                },
                {
                  "kind": "span",
                  "text": "ordered by encoded character sequence.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Codecs",
              "line": 159,
              "column": 7,
              "type": {
                "label": "ZanyBlue.Text.Codecs.Codecs_Type",
                "docHref": "docs/zanyblue.text.codecs.html#L43C9"
              },
              "description": [
              ]
            },
            {
              "label": "Handler",
              "line": 160,
              "column": 7,
              "type": {
                "label": ""
              },
              "description": [
              ]
            },
            {
              "label": "WCh",
              "line": 160,
              "column": 44,
              "type": {
                "label": "Wide_Character"
              },
              "description": [
              ]
            },
            {
              "label": "Encoding",
              "line": 161,
              "column": 44,
              "type": {
                "label": "String"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Iterate_Encodings",
          "line": 165,
          "column": 14,
          "src": "srcs/zanyblue-text-codecs.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 165,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "   "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "procedure"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Iterate_Encodings",
                      "href": "docs/zanyblue.text.codecs.html#L165C14"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 166,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "      "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Codecs",
                      "href": "docs/zanyblue.text.codecs.html#L166C7"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "   "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ":"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Codecs_Type",
                      "href": "docs/zanyblue.text.codecs.html#L176C9"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";"
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 167,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "      "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Handler",
                      "href": "docs/zanyblue.text.codecs.html#L167C7"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ":"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "not"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "null"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "access"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "procedure"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "WCh",
                      "href": "docs/zanyblue.text.codecs.html#L167C44"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ":"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Wide_Character"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";"
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 168,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "                                           "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Encoding",
                      "href": "docs/zanyblue.text.codecs.html#L168C44"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ":"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "String"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ")"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ")"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";"
                    }
                  ]
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "Iterate over the encoding mappings for a Codecs.  The iteration is\n"
                },
                {
                  "kind": "span",
                  "text": "ordered by Unicode code point associated with the mapping.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Codecs",
              "line": 166,
              "column": 7,
              "type": {
                "label": "ZanyBlue.Text.Codecs.Codecs_Type",
                "docHref": "docs/zanyblue.text.codecs.html#L43C9"
              },
              "description": [
              ]
            },
            {
              "label": "Handler",
              "line": 167,
              "column": 7,
              "type": {
                "label": ""
              },
              "description": [
              ]
            },
            {
              "label": "WCh",
              "line": 167,
              "column": 44,
              "type": {
                "label": "Wide_Character"
              },
              "description": [
              ]
            },
            {
              "label": "Encoding",
              "line": 168,
              "column": 44,
              "type": {
                "label": "String"
              },
              "description": [
              ]
            }
          ]
        }
      ],
      "label": "Dispatching subprograms"
    }
  ]
};