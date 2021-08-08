#[cfg(test)]
mod tests {
    use crate::{
        css::{Stylesheet,CSSValue, AttributeSelectorOp, Declaration, Rule, SimpleSelector},
        dom::Element,
        style::{StyledNode,to_styled_node}
    };


    #[test]
    fn test_universal_selector_begaviour() {
        let e = &Element::new(
            "p".to_owned(),
            [("id".to_owned(), "test".to_owned()),
            ("class".to_owned(), "testclass".to_owned())]
            .iter().cloned().collect(),
            vec![],
        );
        assert_eq!(SimpleSelector::UniversalSelector.matches(e), true);
    }
     #[test]
    fn test_type_selector_behaviour() {
        let e = &Element::new(
            "p".to_string(),
            [("id".to_string(), "test".to_string()),
            ("class".to_string(), "testclass".to_string())]
                .iter()
                .cloned()
                .collect(),
            vec![],
        );

        assert_eq!(
            (SimpleSelector::TypeSelector {
                tag_name: "p".into(),
            })
            .matches(e),
            true
        );

        assert_eq!(
            (SimpleSelector::TypeSelector {
                tag_name: "invalid".into(),
            })
            .matches(e),
            false
        );
    }

    #[test]
    fn test_attribute_selector_behaviour() {
        let e = &Element::new(
            "p".to_string(),
            [("id".to_string(), "test".to_string()),
            ("class".to_string(), "testclass".to_string())]
                .iter()
                .cloned()
                .collect(),
            vec![],
        );

        assert_eq!(
            (SimpleSelector::AttributeSelector {
                tag_name: "p".into(),
                attribute: "id".into(),
                value: "test".into(),
                op: AttributeSelectorOp::Eq,
            })
            .matches(e),
            true
        );

        assert_eq!(
            (SimpleSelector::AttributeSelector {
                tag_name: "p".into(),
                attribute: "id".into(),
                value: "invalid".into(),
                op: AttributeSelectorOp::Eq,
            })
            .matches(e),
            false
        );

        assert_eq!(
            (SimpleSelector::AttributeSelector {
                tag_name: "p".into(),
                attribute: "invalid".into(),
                value: "test".into(),
                op: AttributeSelectorOp::Eq,
            })
            .matches(e),
            false
        );

        assert_eq!(
            (SimpleSelector::AttributeSelector {
                tag_name: "invalid".into(),
                attribute: "id".into(),
                value: "test".into(),
                op: AttributeSelectorOp::Eq,
            })
            .matches(e),
            false
        );
    }

    #[test]
    fn test_class_selector_behaviour() {
        let e = &Element::new(
            "p".to_string(),
            [("id".to_string(), "test".to_string()),
            ("class".to_string(), "testclass".to_string())]
                .iter()
                .cloned()
                .collect(),
            vec![],
        );

        assert_eq!(
            (SimpleSelector::ClassSelector {
                class_name: "testclass".into(),
            })
            .matches(e),
            true
        );

        assert_eq!(
            (SimpleSelector::ClassSelector {
                class_name: "invalid".into(),
            })
            .matches(e),
            false
        );
    }

    #[test]
    fn test_to_styled_node_single() {
        let e = &Element::new(
            "p".to_string(),
            [("id".to_string(), "test".to_string())]
                .iter()
                .cloned()
                .collect(),
            vec![],
        );
        let testcases = vec![
            (
                // * { display: block; }
                Stylesheet::new(vec![Rule {
                    selectors: vec![SimpleSelector::UniversalSelector],
                    declarations: vec![Declaration {
                        name: "display".to_string(),
                        value: CSSValue::Keyword("block".to_string()),
                    }],
                }]),
                vec![(
                    "display".to_string(),
                    CSSValue::Keyword("block".to_string()),
                )],
            ),
            (
                // div { display: block; }
                Stylesheet::new(vec![Rule {
                    selectors: vec![SimpleSelector::TypeSelector {
                        tag_name: "div".into(),
                    }],
                    declarations: vec![Declaration {
                        name: "display".into(),
                        value: CSSValue::Keyword("block".to_string()),
                    }],
                }]),
                vec![],
            ),
            (
                // * { display: block; }
                // div { display: inline; }
                Stylesheet::new(vec![
                    Rule {
                        selectors: vec![SimpleSelector::UniversalSelector],
                        declarations: vec![Declaration {
                            name: "display".to_string(),
                            value: CSSValue::Keyword("block".into()),
                        }],
                    },
                    Rule {
                        selectors: vec![SimpleSelector::TypeSelector {
                            tag_name: "div".into(),
                        }],
                        declarations: vec![Declaration {
                            name: "display".into(),
                            value: CSSValue::Keyword("inline".into()),
                        }],
                    },
                ]),
                vec![(
                    "display".to_string(),
                    CSSValue::Keyword("block".to_string()),
                )],
            ),
            (
                // * { display: block; }
                // p { display: inline; testname: testvalue; }
                Stylesheet::new(vec![
                    Rule {
                        selectors: vec![SimpleSelector::UniversalSelector],
                        declarations: vec![Declaration {
                            name: "display".to_string(),
                            value: CSSValue::Keyword("block".into()),
                        }],
                    },
                    Rule {
                        selectors: vec![SimpleSelector::TypeSelector {
                            tag_name: "p".into(),
                        }],
                        declarations: vec![
                            Declaration {
                                name: "display".into(),
                                value: CSSValue::Keyword("inline".into()),
                            },
                            Declaration {
                                name: "testname".into(),
                                value: CSSValue::Keyword("testvalue".into()),
                            },
                        ],
                    },
                ]),
                vec![
                    ("display".into(), CSSValue::Keyword("inline".into())),
                    ("testname".into(), CSSValue::Keyword("testvalue".into())),
                ],
            ),
            (
                // * { display: block; }
                // p[id=hello] { testname: testvalue; }
                Stylesheet::new(vec![
                    Rule {
                        selectors: vec![SimpleSelector::UniversalSelector],
                        declarations: vec![Declaration {
                            name: "display".to_string(),
                            value: CSSValue::Keyword("block".into()),
                        }],
                    },
                    Rule {
                        selectors: vec![SimpleSelector::AttributeSelector {
                            tag_name: "p".into(),
                            op: AttributeSelectorOp::Eq,
                            attribute: "id".into(),
                            value: "hello".into(),
                        }],
                        declarations: vec![Declaration {
                            name: "testname".into(),
                            value: CSSValue::Keyword("testvalue".into()),
                        }],
                    },
                ]),
                vec![("display".into(), CSSValue::Keyword("block".into()))],
            ),
            (
                // * { display: block; }
                // p[id=hello] { testname: testvalue; }
                Stylesheet::new(vec![
                    Rule {
                        selectors: vec![SimpleSelector::UniversalSelector],
                        declarations: vec![Declaration {
                            name: "display".to_string(),
                            value: CSSValue::Keyword("block".into()),
                        }],
                    },
                    Rule {
                        selectors: vec![SimpleSelector::AttributeSelector {
                            tag_name: "p".into(),
                            op: AttributeSelectorOp::Eq,
                            attribute: "id".into(),
                            value: "test".into(),
                        }],
                        declarations: vec![Declaration {
                            name: "testname".into(),
                            value: CSSValue::Keyword("testvalue".into()),
                        }],
                    },
                ]),
                vec![
                    ("display".into(), CSSValue::Keyword("block".into())),
                    ("testname".into(), CSSValue::Keyword("testvalue".into())),
                ],
            ),
        ];

        for (stylesheet, properties) in testcases {
            assert_eq!(
                to_styled_node(e, &stylesheet),
                Some(StyledNode {
                    node_type: &e.node_type,
                    properties: properties.iter().cloned().collect(),
                    children: vec![],
                })
            );
        }
    }

    #[test]
    fn test_to_styled_node_nested() {
        let parent = &Element::new(
            "div".to_string(),
            [("id".to_string(), "test".to_string())]
                .iter()
                .cloned()
                .collect(),
            vec![Element::new(
                "p".to_string(),
                [("id".to_string(), "test".to_string())]
                    .iter()
                    .cloned()
                    .collect(),
                vec![],
            )],
        );
        let child_node_type = Element::new(
            "p".to_string(),
            [("id".to_string(), "test".to_string())]
                .iter()
                .cloned()
                .collect(),
            vec![],
        )
        .node_type;

        {
            // * { display: block; }
            let stylesheet = Stylesheet::new(vec![Rule {
                selectors: vec![SimpleSelector::UniversalSelector],
                declarations: vec![Declaration {
                    name: "display".to_string(),
                    value: CSSValue::Keyword("block".to_string()),
                }],
            }]);

            assert_eq!(
                to_styled_node(parent, &stylesheet),
                Some(StyledNode {
                    node_type: &parent.node_type,
                    properties: [(
                        "display".to_string(),
                        CSSValue::Keyword("block".to_string()),
                    )]
                    .iter()
                    .cloned()
                    .collect(),
                    children: vec![StyledNode {
                        node_type: &child_node_type,
                        properties: [(
                            "display".to_string(),
                            CSSValue::Keyword("block".to_string()),
                        )]
                        .iter()
                        .cloned()
                        .collect(),
                        children: vec![],
                    }],
                })
            );
        }

        {
            // p { display: block; }
            let stylesheet = Stylesheet::new(vec![Rule {
                selectors: vec![SimpleSelector::TypeSelector {
                    tag_name: "p".into(),
                }],
                declarations: vec![Declaration {
                    name: "display".to_string(),
                    value: CSSValue::Keyword("block".to_string()),
                }],
            }]);

            assert_eq!(
                to_styled_node(parent, &stylesheet),
                Some(StyledNode {
                    node_type: &parent.node_type,
                    properties: [].iter().cloned().collect(),
                    children: vec![StyledNode {
                        node_type: &child_node_type,
                        properties: [(
                            "display".to_string(),
                            CSSValue::Keyword("block".to_string()),
                        )]
                        .iter()
                        .cloned()
                        .collect(),
                        children: vec![],
                    }],
                })
            );
        }
    }

    #[test]
    fn test_to_styled_node_nested_single() {
        let parent = &Element::new(
            "div".to_string(),
            [("id".to_string(), "test".to_string())]
                .iter()
                .cloned()
                .collect(),
            vec![],
        );

        // p { display: none; }
        let stylesheet = Stylesheet::new(vec![Rule {
            selectors: vec![SimpleSelector::TypeSelector {
                tag_name: "div".into(),
            }],
            declarations: vec![Declaration {
                name: "display".to_string(),
                value: CSSValue::Keyword("none".to_string()),
            }],
        }]);

        assert_eq!(to_styled_node(parent, &stylesheet), None);
    }

    #[test]
    fn test_to_styled_node_nested_none() {
        let parent = &Element::new(
            "div".to_string(),
            [("id".to_string(), "test".to_string())]
                .iter()
                .cloned()
                .collect(),
            vec![Element::new(
                "p".to_string(),
                [("id".to_string(), "test".to_string())]
                    .iter()
                    .cloned()
                    .collect(),
                vec![],
            )],
        );

        // p { display: none; }
        let stylesheet = Stylesheet::new(vec![Rule {
            selectors: vec![SimpleSelector::TypeSelector {
                tag_name: "p".into(),
            }],
            declarations: vec![Declaration {
                name: "display".to_string(),
                value: CSSValue::Keyword("none".to_string()),
            }],
        }]);

        assert_eq!(
            to_styled_node(parent, &stylesheet),
            Some(StyledNode {
                node_type: &parent.node_type,
                properties: [].iter().cloned().collect(),
                children: vec![],
            })
        );
    }
}
