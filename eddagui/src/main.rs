extern crate glium;
#[macro_use]
extern crate imgui;
extern crate edda;
extern crate imgui_glium_renderer;

use std::env;
use std::fs::File;
use std::io::prelude::*;

use imgui::*;

use edda::token::TokenType;
use edda::ast::{Expression, Statement};
use edda::parser::{parse_tokens, ParseError};
use edda::scanner::{scan_tokens, ScanError};

mod support;

const CLEAR_COLOR: [f32; 4] = [0.01, 0.01, 0.01, 1.0];

struct State {
    show_grid: bool,
}

impl Default for State {
    fn default() -> Self {
        State { show_grid: false }
    }
}

enum NodeType {
    Expression(Expression),
    Statement(Statement),
}

enum InputType {
    Normal,
    Previous,
    Statement,
    Expression,
    OptionalExpression,
    String,
}

enum OutputType {
    Normal,
    Next,
    Expression,
}

struct Node {
    position: (f32, f32),
    size: (f32, f32),
    inputs: Vec<InputType>,
    outputs: Vec<OutputType>,
    data: NodeType,
    id: String,
}

impl Node {
    fn get_input_slot_pos(&self, index: usize) -> (f32, f32) {
        (self.position.0 - 8.0,
        self.position.1 + (index as f32) * 20.0 + 16.0)
    }

    fn get_output_slot_pos(&self, index: usize) -> (f32, f32) {
        (self.position.0 + self.size.0 + 8.0,
        self.position.1 + (index as f32) * 20.0 + 16.0)
    }
}

struct Link {
    input_index: usize,
    output_index: usize,
    input_slot: usize,
    output_slot: usize,
}

fn create_child_node(expr: &Expression, nodes: &mut Vec<Node>, links: &mut Vec<Link>, mut cursor_x: f32, mut cursor_y: f32) -> usize {
    let assume_height = 128.0;

    match expr {
        &Expression::Literal(ref lit) => {
            let title = "Literal".to_owned();
            let id = format!("{}-{}", title, nodes.len());

            cursor_y += assume_height;
            let node = Node {
                id: id,
                position: (cursor_x, cursor_y),
                size: (0.0, 0.0),
                inputs: Vec::new(),
                outputs: vec![OutputType::Expression],
                data: NodeType::Expression(expr.clone()),
            };

            nodes.push(node);
            nodes.len() - 1
        },
        &Expression::FunctionDeclaration(ref tokens, ref body) => {
            // recursivly create child node for the function body
            cursor_y += assume_height;
            let child_node_id = create_child_node(body, nodes, links, cursor_x, cursor_y);

            let title = "Function".to_owned();
            let id = format!("{}-{}", title, nodes.len());

            let node = Node {
                id: id,
                position: (cursor_x, cursor_y),
                size: (0.0, 0.0),
                inputs: Vec::new(),
                outputs: vec![OutputType::Expression],
                data: NodeType::Expression(expr.clone()),
            };

            // create link to the child node
            let child_link = Link {
                input_index: child_node_id,
                output_index: nodes.len(),
                input_slot: 0,
                output_slot: 2,
            };

            links.push(child_link);

            nodes.push(node);
            nodes.len() - 1
        },
        &Expression::BlockExpression(ref statements, ref expression) => {
            let title = "BlockExpression".to_owned();
            let id = format!("{}-{}", title, nodes.len());

            cursor_y += assume_height;
            let node = Node {
                id: id,
                position: (cursor_x, cursor_y),
                size: (0.0, 0.0),
                inputs: Vec::new(),
                outputs: vec![OutputType::Expression],
                data: NodeType::Expression(expr.clone()),
            };

            nodes.push(node);
            nodes.len() - 1
        },
        &Expression::Unary { ref operator, ref expr } => {
            0
        },
        &Expression::Binary { ref operator, ref left, ref right} => {
            0
        },
        &Expression::Grouping(ref expr) => {
            0
        },
        &Expression::Variable(ref id) => {
            let title = "Variable".to_owned();
            let id = format!("{}-{}", title, nodes.len());

            cursor_y += 200.0;
            let node = Node {
                id: id,
                position: (cursor_x, cursor_y),
                size: (0.0, 0.0),
                inputs: Vec::new(),
                outputs: vec![OutputType::Expression],
                data: NodeType::Expression(expr.clone()),
            };

            nodes.push(node);
            nodes.len() - 1
        }
    }
}

fn build_nodes_and_links(statements: &Vec<Statement>) -> (Vec<Node>, Vec<Link>) {
    let mut nodes = Vec::new();
    let mut links = Vec::new();

    let mut previous_statement_index: isize = -1;

    let mut cursor_x = 64.0;
    let mut cursor_y = 64.0;

    for statement in statements {
        let mut inputs = Vec::new();
        let mut outputs = Vec::new();
        let title = "Statement".to_owned();

        inputs.push(InputType::Previous);
        outputs.push(OutputType::Next);

        match statement {
            &Statement::Expression(..) => {
                inputs.push(InputType::Expression);
            },
            &Statement::Print(ref expr) => {
                inputs.push(InputType::Expression);

                let child_node_id = create_child_node(&expr, &mut nodes, &mut links, cursor_x, cursor_y);
                let child_link = Link {
                    input_index: child_node_id,
                    output_index: nodes.len(),
                    input_slot: 0,
                    output_slot: 1,
                };

                links.push(child_link);
            },
            &Statement::VarDeclaration(_, ref initializer) => {
                inputs.push(InputType::String);
                inputs.push(InputType::OptionalExpression);

                if let Some(ref expr) = initializer {
                    let child_node_id = create_child_node(&expr, &mut nodes, &mut links, cursor_x, cursor_y);
                    let child_link = Link {
                        input_index: child_node_id,
                        output_index: nodes.len(),
                        input_slot: 0,
                        output_slot: 2,
                    };

                    links.push(child_link);
                }
            },
            &Statement::GlobalDeclaration(..) => {
                inputs.push(InputType::String);
                inputs.push(InputType::Expression);
            },
            &Statement::BlockStatement(..) => {
                inputs.push(InputType::Statement);
            }
        };

        let id = format!("{}-{}", title, nodes.len());
        let statement_node = Node {
            position: (cursor_x, cursor_y),
            size: (0.0, 0.0),
            inputs: inputs,
            outputs: outputs,
            data: NodeType::Statement(statement.clone()),
            id: id,
        };

        // create link to last statement node
        if previous_statement_index > -1 {
            let link = Link {
                input_index: previous_statement_index as usize,
                output_index: nodes.len(),
                input_slot: 0,
                output_slot: 0,
            };

            links.push(link);
        }

        nodes.push(statement_node);
        previous_statement_index = (nodes.len() as isize) - 1;
        cursor_x += 120.0 + 64.0;
    }

    (nodes, links)
}

fn main() {
    let args: Vec<_> = env::args().collect();
    let mut filename = "../examples/helloworld.edda";

    if args.len() > 1 {
        filename = &args[1];
    }

    let mut f = File::open(filename).expect("file not found");

    let mut script = String::new();
    f.read_to_string(&mut script)
        .expect("something went wrong reading the file");

    let tokens = scan_tokens(&script).unwrap();
    let statements = match parse_tokens(&tokens) {
        Ok(statements) => statements,
        Err(parse_errors) => {
            for parse_error in parse_errors {
                edda::print_parse_error(&parse_error, &script);
                println!("");
            }
            return;
        }
    };

    let (mut nodes, links) = build_nodes_and_links(&statements);
    let mut state = State::default();
    let mut scrolling = (0.0, 0.0);

    support::run("hello_world.rs".to_owned(), CLEAR_COLOR, |ui| {
        // list of statements
        ui.window(im_str!("Nodes"))
            .size((200.0, 736.0), ImGuiCond::Always)
            .position((16.0, 16.0), ImGuiCond::Always)
            .build(|| {
                for (i, stmt) in statements.iter().enumerate() {
                    let title = match stmt {
                        Statement::Expression(..) => "Expression",
                        Statement::Print(..) => "Print",
                        Statement::VarDeclaration(..) => "VarDeclaration",
                        Statement::GlobalDeclaration(..) => "GlobalDeclaration",
                        Statement::BlockStatement(..) => "BlockStatement",
                    };

                    if ui.selectable(
                        im_str!("{}##{}", title, i),
                        false,
                        ImGuiSelectableFlags::empty(),
                        ImVec2::new(0.0, 0.0),
                    ) {}
                }
            });

        ui.window(im_str!("Source"))
            .size((776.0, 220.0), ImGuiCond::Always)
            .position((232.0, 16.0), ImGuiCond::Always)
            .horizontal_scrollbar(true)
            .build(|| {
                ui.text(im_str!("{}", script));
            });

        ui.window(im_str!("Graph"))
            .size((776.0, 500.0), ImGuiCond::Always)
            .position((232.0, 252.0), ImGuiCond::Always)
            .scroll_bar(false)
            .scrollable(false)
            .build(|| {
                ui.text(im_str!(
                    "Hold middle mouse button to scroll ({}, {})",
                    scrolling.0,
                    scrolling.1
                ));
                ui.same_line(ui.get_window_size().0 - 100.0);
                ui.checkbox(im_str!("Show grid"), &mut state.show_grid);
                ui.child_frame(im_str!("child"), (0.0, 0.0))
                    .show_scrollbar(false)
                    .show_borders(false)
                    .build(|| {
                        ui.push_item_width(120.0);

                        let cursor_pos = ui.get_cursor_screen_pos();
                        let offset = (cursor_pos.0 + scrolling.0, cursor_pos.1 + scrolling.1);

                        let draw_list = ui.get_window_draw_list();

                        if state.show_grid {
                            let grid_color = (0.8, 0.8, 0.8, 0.3);
                            let grid_size = 64.0;

                            let win_pos = ui.get_cursor_screen_pos();
                            let canvas_size = ui.get_window_size();

                            let mut x = scrolling.0 % grid_size;
                            while x < canvas_size.0 {
                                draw_list
                                    .add_line(
                                        (win_pos.0 + x, win_pos.1),
                                        (win_pos.0 + x, win_pos.1 + canvas_size.1),
                                        grid_color,
                                    )
                                    .build();
                                x += grid_size;
                            }

                            let mut y = scrolling.1 % grid_size;
                            while y < canvas_size.1 {
                                draw_list
                                    .add_line(
                                        (win_pos.0, win_pos.1 + y),
                                        (win_pos.0 + canvas_size.0, win_pos.1 + y),
                                        grid_color,
                                    )
                                    .build();
                                y += grid_size;
                            }
                        }

                        draw_nodes(ui, &draw_list, &mut nodes, &links, &offset);
                    });

                    if ui.is_item_hovered() && ui.imgui().is_mouse_dragging(ImMouseButton::Middle) {
                        let mouse_delta = ui.imgui().mouse_delta();
                        scrolling = (
                            scrolling.0 + mouse_delta.0,
                            scrolling.1 + mouse_delta.1);
                    }
            });

        true
    });
}

fn draw_nodes(ui: &Ui, draw_list: &WindowDrawList, nodes: &mut Vec<Node>, links: &Vec<Link>, offset: &(f32, f32)) -> () {
    let node_window_padding = 8.0;

    draw_list.channels_split(2, |channels| {
        for link in links {
            let input_node = nodes.get(link.input_index).unwrap();
            let output_node = nodes.get(link.output_index).unwrap();
            let input_slot_pos = input_node.get_output_slot_pos(link.input_slot);
            let output_slot_pos = output_node.get_input_slot_pos(link.output_slot);

            let p1 = (offset.0 + input_slot_pos.0, offset.1 + input_slot_pos.1);
            let p2 = (offset.0 + output_slot_pos.0, offset.1 + output_slot_pos.1);

            draw_list.add_bezier_curve(
                p1,
                (p1.0 + 50.0, p1.1),
                (p2.0 - 50.0, p2.1),
                p2,
                (1.0, 1.0, 1.0, 1.0))
                .thickness(2.0)
                .build();
        }

        for (i, node) in nodes.iter_mut().enumerate() {
            let node_rect_min = (offset.0 + node.position.0,
                offset.1 + node.position.1);

            ui.with_id(im_str!("node-{}", i), || {
                channels.set_current(1);
                ui.set_cursor_screen_pos((
                    node_rect_min.0 + node_window_padding,
                    node_rect_min.1 + node_window_padding,
                ));
                ui.group(|| {
                    ui.with_style_var(StyleVar::ItemSpacing(ImVec2::new(0.0, 4.0)), || {
                        ui.with_color_var(ImGuiCol::Text, (1.0, 1.0, 1.0, 1.0), || {
                            ui.text(im_str!("{}", get_node_title(&node)));
                        });
                        ui.with_color_var(ImGuiCol::Text, (0.8, 0.8, 0.8, 1.0), || {
                            match node.data {
                                NodeType::Statement(ref stmt) => draw_statement_node_contents(ui, stmt),
                                NodeType::Expression(ref expr) => draw_expression_node_contents(ui, expr),
                            };
                        });
                    });
                });

                let node_rect = ui.get_item_rect_size();
                node.size = (
                    node_rect.0 + node_window_padding * 2.0,
                    node_rect.1 + node_window_padding * 2.0,
                );
                let node_rect_max = (
                    node_rect_min.0 + node.size.0,
                    node_rect_min.1 + node.size.1,
                );

                channels.set_current(0);
                ui.set_cursor_screen_pos(node_rect_min);
                ui.invisible_button(im_str!("node"), node.size);
                if ui.is_item_hovered() {
                    // TODO: set node_hovered and open_context_menu
                }

                if ui.is_item_active() {
                    let mouse_delta = ui.imgui().mouse_delta();
                    node.position = (
                        node.position.0 + mouse_delta.0,
                        node.position.1 + mouse_delta.1);
                }

                let node_bg_color = get_node_color(node);
                draw_list
                    .add_rect(node_rect_min, node_rect_max, node_bg_color)
                    .rounding(4.0)
                    .filled(true)
                    .build();
                draw_list
                    .add_rect(node_rect_min, node_rect_max, (0.2, 0.2, 0.2, 0.9))
                    .rounding(4.0)
                    .thickness(3.0)
                    .build();
            });
        }

    });
}

fn get_node_color(node: &Node) -> (f32, f32, f32, f32) {
    match node.data {
        NodeType::Expression(..) => (0.5, 0.4, 0.1, 0.9),
        NodeType::Statement(..) => (0.1, 0.1, 0.4, 0.9),
        _ => (0.2, 0.2, 0.2, 0.9),
    }
}

fn get_node_title(node: &Node) -> &str {
    match &node.data {
        &NodeType::Statement(ref stmt) => match stmt {
            &Statement::Expression(_) => "ExprStmt",
            &Statement::Print(_) => "Print",
            &Statement::VarDeclaration(..) => "Binding",
            &Statement::GlobalDeclaration(..) => "GlobBinding",
            &Statement::BlockStatement(..) => "BlockStmt",
        },
        &NodeType::Expression(ref expr) => match expr {
            &Expression::Variable(_) => "Variable",
            &Expression::Literal(_) => "Literal",
            &Expression::FunctionDeclaration(..) => "Function",
            _ => "Expression"
        },
    }
}

fn draw_statement_node_contents(ui: &Ui, stmt: &Statement) -> () {
    match stmt {
        &Statement::Expression(ref expr) => {
            ui.text(im_str!("Expression"));
        }
        &Statement::Print(ref expr) => {
            ui.text(im_str!("Expression"));
        }
        &Statement::VarDeclaration(ref id, ref initializer) => {
            let mut identifier = ImString::with_capacity(32);
            identifier.push_str(id);
            ui.input_text(
                im_str!("##no_label"),
                &mut identifier,
            ).build();
            ui.text(im_str!("Initializer"));
        }
        &Statement::GlobalDeclaration(
            ref id,
            ref initializer,
        ) => {
            let mut identifier = ImString::with_capacity(32);
            identifier.push_str(id);
            ui.text("Id");
            ui.same_line(40.0);
            ui.input_text(
                im_str!("##no_label"),
                &mut identifier,
            ).build();
            ui.text(im_str!("Initializer"));
        }
        &Statement::BlockStatement(ref statements) => {
            ui.text(im_str!("Statements"));
        }
    }
}

fn draw_expression_node_contents(ui: &Ui, expr: &Expression) -> () {
    match expr {
        &Expression::Literal(ref lit) => {
            ui.text(im_str!("{}", lit));
        },
        &Expression::FunctionDeclaration(ref params, _) => {
            let param_ids = params.iter().map(|p| {
                match p.ttype {
                    TokenType::Identifier(ref id) => id.clone(),
                    _ => panic!("Not a Identifier: {:?}", p)
                }
            }).collect::<Vec<_>>();

            if param_ids.len() > 0 {
                ui.text(im_str!("{}", param_ids.as_slice().join(", ")));
            } else {
                ui.text(im_str!("No params"));
            }

            ui.text(im_str!("Body"));
        },
        &Expression::BlockExpression(..) => (),
        &Expression::Unary {..} => (),
        &Expression::Binary {..} => (),
        &Expression::Grouping(..) => (),
        &Expression::Variable(ref id) => {
            let mut identifier = ImString::with_capacity(32);
            identifier.push_str(id);
            ui.input_text(
                im_str!("##no_label"),
                &mut identifier,
            ).build();
        },
    }
}