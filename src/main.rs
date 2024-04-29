use scryer_prolog::machine::*;
use scryer_prolog::machine::parsed_results::*;
use slint::{ModelRc, StandardListViewItem, VecModel};
use std::rc::Rc;

fn main() {
    let prolog_code = include_str!("../prolog/riscv64.pl");
    let mut machine = Machine::new_lib();
    machine.load_module_string("riscv64", prolog_code.to_string());
    let results = machine.run_query("all_instr(X).".to_string());
    let mut instructions: Vec<StandardListViewItem> = Vec::new();
    match results {
	Ok(query_res) => match query_res {
	    QueryResolution::Matches(matches) => {
		let bindings = matches[0].bindings.clone();
		if let Some(value) = bindings.get("X") {
		    match value {
			Value::List(list) => {
			    for x in list {
				match x {
				    Value::String(string) => {
					instructions.push(StandardListViewItem::from(string.as_str()));
				    },
				    _ => unimplemented!()
				}
			    }
			},
			_ => unimplemented!()
		    }
		}
	    }
	    _ => unimplemented!()
	},
	_ => unimplemented!()
    };
    
    
    let window = MainWindow::new().unwrap();
    let model = Rc::new(VecModel::from(instructions));
    window.set_instructions(ModelRc::from(model.clone()));

    window.on_instr_doc(move |instr| {
	let mut output = String::new();
	match machine.run_query(format!("phrase(instr_doc_({}), X).", instr.clone())) {
	    Ok(query_res) => match query_res {
		QueryResolution::Matches(matches) => {
		    let bindings = matches[0].bindings.clone();
		    if let Some(value) = bindings.get("X") {
			match value {
			    Value::String(string) => {
				output = string.replace("\\n", "\n");
			    }
			    _ => ()
			}
		    }
		}
		_ => ()
	    }
	    _ => ()
	};
	output.into()
    });

    window.run().unwrap();
}

slint::slint! {
    import { HorizontalBox, StandardListView, TabWidget, TextEdit } from "std-widgets.slint";
    export component MainWindow inherits Window {
	in-out property<[StandardListViewItem]> instructions: [{text: "Hola"}, {text: "Adios"}];
	callback instr_doc(string) -> string;
	TabWidget {
	    Tab {
		title: "RISC-V Reference";
		property<string> referenceText: "";
		HorizontalLayout {
		    StandardListView {
			width: 150px;
			model: instructions;
			current-item-changed(i) => {
			    referenceText = instr_doc(instructions[i].text);
			}
		    }
		    TextEdit {
			horizontal-stretch: 1;
			text: referenceText;
			read-only: true;
		    }
		}
	    }
	}
    }
}
