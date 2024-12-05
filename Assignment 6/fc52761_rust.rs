use std::collections::HashMap;
use std::thread;
use std::sync::mpsc;

enum Json {
    Number(i32),
    String(String),
    Boolean(bool),
    Array(Vec<Json>),
    Object(HashMap<String, Json>),
    Null
}

enum JC {
    Number(i32),
    String(String),
    Boolean(bool),
    Null,
    Channel(mpsc::Receiver<JC>),
    VS,
    OS
}

enum Accessor {
    LabelAccess(String, Box<Accessor>),
    ArrayAccess(i32, Box<Accessor>),
    Map(Box<Accessor>),
    End
}

fn serialise_json(json: &Json, sender: mpsc::Sender<JC>) {
    rec_serialise_json(json, &sender)
}

fn rec_serialise_json(json: &Json, sender: &mpsc::Sender<JC>) {
    match json {
        Json::Number(n) => sender.send(JC::Number(*n)).unwrap(),
        Json::String(s) => sender.send(JC::String(s.to_string())).unwrap(),
        Json::Boolean(b) => sender.send(JC::Boolean(*b)).unwrap(),
        Json::Array(arr) => {
                let (v_sender, v_receiver) = mpsc::channel();
                sender.send(JC::Channel(v_receiver)).unwrap();

                v_sender.send(JC::VS).unwrap();

                for e in arr {
                    rec_serialise_json(e, &v_sender)
                }
            },
        Json::Object(obj) => {
                let (o_sender, o_receiver) = mpsc::channel();
                sender.send(JC::Channel(o_receiver)).unwrap();

                o_sender.send(JC::OS).unwrap();

                for (key, value) in obj {
                    o_sender.send(JC::String(key.to_string())).unwrap();
                    rec_serialise_json(value, &o_sender)
                }
            },
        Json::Null => sender.send(JC::Null).unwrap()
    }
}

fn deserialise_json(receiver: mpsc::Receiver<JC>) -> Json {
    rec_deserialise_json(receiver.recv().unwrap(), &receiver)
}

fn rec_deserialise_json(jc: JC, c: &mpsc::Receiver<JC>) -> Json {
    match jc {
        JC::Number(n) => return Json::Number(n),
        JC::String(s) => return Json::String(s),
        JC::Boolean(b) => return Json::Boolean(b),
        JC::Null => return Json::Null,
        JC::Channel(c) => {
            let next_jc: JC = c.recv().unwrap();
            return rec_deserialise_json(next_jc, &c);
        },
        JC::VS => {
            let mut arr: Vec<Json> = Vec::new();

            for v_jc in c {
                arr.push(rec_deserialise_json(v_jc, c))
            }
            return Json::Array(arr)
        },
        JC::OS => {
            let mut obj: HashMap<String, Json> = HashMap::new();

            for o_jc in c {
                obj.insert(get_string_jc(&o_jc), rec_deserialise_json(c.recv().unwrap(), c));
            }
            return Json::Object(obj)
        }
    }
}

fn eval(accessor: &Accessor,
    receiver: mpsc::Receiver<JC>,
    sender: mpsc::Sender<JC>) {
        rec_eval(receiver.recv().unwrap(), accessor, &receiver, &sender);
    }

fn rec_eval(jc: JC, accessor: &Accessor,
    receiver: &mpsc::Receiver<JC>, sender: &mpsc::Sender<JC>) {
        match jc {
            JC::Number(n) => sender.send(JC::Number(n)).unwrap(),
            JC::String(s) => sender.send(JC::String(s.to_string())).unwrap(),
            JC::Boolean(b) => sender.send(JC::Boolean(b)).unwrap(),
            JC::Null => sender.send(JC::Null).unwrap(),
            JC::Channel(c) => rec_eval(c.recv().unwrap(), &accessor, &c, &sender),
            JC::VS => {
                match &accessor {
                    Accessor::ArrayAccess(n, cont) => {
                        let mut i: i32 = 0;
                        for v_jc in receiver {
                            if i == *n {
                                rec_eval(v_jc, cont, receiver, sender);
                                break;
                            } else {
                                i += 1;
                            }
                        }
                    },
                    Accessor::Map(app) => {
                        let (v_sender1, v_receiver1) = mpsc::channel();

                        sender.send(JC::Channel(v_receiver1)).unwrap();
                        v_sender1.send(JC::VS).unwrap();

                        for v_jc in receiver {
                            rec_eval(v_jc, &app, &receiver, &v_sender1);
                        }
                    },
                    Accessor::End => {
                        let (v_sender1, v_receiver1) = mpsc::channel();

                        sender.send(JC::Channel(v_receiver1)).unwrap();
                        v_sender1.send(JC::VS).unwrap();

                        for v_jc in receiver {
                            rec_eval(v_jc, &accessor, &receiver, &v_sender1);
                        }
                    },
                    _ => panic!("Accessor not applicable")
                }
            },
            JC::OS => {
                match &accessor {
                    Accessor::LabelAccess(l, accessor) => {    
                        for o_jc in receiver {
                            let key = o_jc;
                            let value = receiver.recv().unwrap();
                            if get_string_jc(&key) == l.to_string() {
                                rec_eval(value, &accessor, &receiver, &sender);
                                break;
                            }
                        }
                    },
                    Accessor::End => {
                        let (o_sender1, o_receiver1) = mpsc::channel();
    
                        sender.send(JC::Channel(o_receiver1)).unwrap();
                        o_sender1.send(JC::OS).unwrap();
    
                        for o_jc in receiver {
                            o_sender1.send(o_jc).unwrap();
                            rec_eval(receiver.recv().unwrap(), &accessor, &receiver, &o_sender1)
                        }
                    },
                    _ => panic!("Accessor not applicable")
                }
            }
        }
}


fn main() {
    let json: Json = Json::Object(HashMap::from([
        ("name".to_string(), Json::String("Jason Ray".to_string())),
        ("profession".to_string(), Json::String("Software Engineer".to_string())),
        ("age".to_string(), Json::Number(31)),
        ("address".to_string(), Json::Object(HashMap::from([
            ("city".to_string(), Json::String("New York".to_string())),
            ("postelCode".to_string(), Json::Number(64780)),
            ("country".to_string(), Json::String("USA".to_string()))
        ]))),
        ("languages".to_string(), Json::Array(Vec::from([
            Json::String("Java".to_string()),
            Json::String("Node.js".to_string()),
            Json::String("JavaScript".to_string()),
            Json::String("JSON".to_string())
        ]))),
        ("socialProfiles".to_string(), Json::Array(Vec::from([
            Json::Object(HashMap::from([
                ("name".to_string(), Json::String("Twitter".to_string())),
                ("link".to_string(), Json::String("https://twitter.com".to_string()))
            ])),
            Json::Object(HashMap::from([
                ("name".to_string(), Json::String("Facebook".to_string())),
                ("link".to_string(), Json::String("https://facebook.com".to_string()))
            ]))
        ])))
    ]));

    //let accessor: Accessor = Accessor::End;
    /*let accessor: Accessor = Accessor::LabelAccess(
        "socialProfiles".to_string(), Box::new(Accessor::End)
    );*/
    /*let accessor: Accessor = Accessor::LabelAccess(
        "socialProfiles".to_string(),
        Box::new(Accessor::ArrayAccess(
            0, Box::new(Accessor::End)
        ))
    );*/
    let accessor: Accessor = Accessor::LabelAccess(
        "socialProfiles".to_string(),
        Box::new(Accessor::Map(Box::new(Accessor::LabelAccess(
            "name".to_string(), 
            Box::new(Accessor::End))
        )))
    );

    let (sender, receiver) = mpsc::channel();
    let (sender1, receiver1) = mpsc::channel();

    thread::spawn(move || serialise_json(&json, sender));
    thread::spawn(move || eval(&accessor, receiver, sender1));
    print_json(&deserialise_json(receiver1));
}



// FUNÇÕES AUXILIARES
fn print_json(j_val: &Json) {
    match j_val {
        Json::Number(n) => print!("{}", n),
        Json::String(s) => print!("{}", s),
        Json::Boolean(b) => print!("{}", b),
        Json::Array(arr) => {
                print!("[");
                for e in arr {
                    print_json(e);
                    print!(", ");
                }
                print!("]");
            },
        Json::Object(obj) => {
                println!("{}", "\n{");
                for (key, value) in obj {
                    print!("{}: ", key);
                    print_json(value);
                    println!(", ");
                }
                println!("{}", "}");
            },
        Json::Null => print!("null")
    }
}

fn get_string_jc(jc: &JC) -> String {
    match jc {
        JC::String(s) => s.to_string(),
        _ => "Not a String".to_string()
    }
}