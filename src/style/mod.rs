use crate::{
    css::{CSSValue, Rule, SimpleSelector, Stylesheet},
    dom::{Node, NodeType},
};

mod tests;

use std::collections::HashMap;

pub type PropertyMap = HashMap<String, CSSValue>;

#[derive(Debug, PartialEq)]
pub enum Display {
    Inline,
    Block,
    None,
}

impl Rule {
    pub fn matches(&self, n: &Box<Node>) -> bool {
        self.selectors.iter().any(|s| s.matches(n))
    }
}

impl SimpleSelector {
    pub fn matches(&self, n: &Box<Node>) -> bool {
        use crate::css::AttributeSelectorOp;
        use SimpleSelector::*;

        match self {
            UniversalSelector => true,
            TypeSelector { tag_name } => match n.node_type {
                NodeType::Element(ref e) => e.tag_name.as_str() == tag_name,
                _ => false,
            },
            AttributeSelector {
                tag_name,
                op,
                attribute,
                value,
            } => {
                if let NodeType::Element(ref e) = n.node_type {
                    e.tag_name.as_str() == tag_name
                        && if let AttributeSelectorOp::Eq = op {
                            e.attributes.get(attribute) == Some(value)
                        } else {
                            e.attributes
                                .get(attribute)
                                .map(|v| v.split_ascii_whitespace().find(|v| v == value).is_some())
                                .unwrap_or(false)
                        }
                } else {
                    false
                }
            }
            ClassSelector { class_name } => {
                if let NodeType::Element(ref e) = n.node_type {
                    e.attributes.get("class") == Some(class_name)
                } else {
                    false
                }
            }
        }
    }
}

/// `StyleNode` wraps `Node` with related CSS properties.
/// It forms a tree as `Node` does.
#[derive(Debug, PartialEq)]
pub struct StyledNode<'a> {
    pub node_type: &'a NodeType,
    pub children: Vec<StyledNode<'a>>,
    pub properties: PropertyMap,
}

pub fn to_styled_node<'a>(node: &'a Box<Node>, stylesheet: &Stylesheet) -> Option<StyledNode<'a>> {
    todo!("you need to implement this");
}

impl<'a> StyledNode<'a> {
    pub fn display(&self) -> Display {
        match self.properties.get("display") {
            Some(CSSValue::Keyword(s)) => match s.as_str() {
                "block" => Display::Block,
                "none" => Display::None,
                _ => Display::Inline,
            },
            _ => Display::Inline,
        }
    }
}
