use hime_redist::symbols::Symbol;

pub const TERMINALS: &[Symbol] = &[
    Symbol { id: 0x0001, name: "ε" },
    Symbol { id: 0x0002, name: "$" },
    Symbol { id: 0x0003, name: "WHITE_SPACE" },
    Symbol { id: 0x0004, name: "SEPARATOR" },
    Symbol { id: 0x0005, name: "INTEGER" },
    Symbol { id: 0x0006, name: "REAL" },
    Symbol { id: 0x0007, name: "NUMBER" },
    Symbol { id: 0x0008, name: "LL" },
    Symbol { id: 0x0009, name: "LR" },
    Symbol { id: 0x000A, name: "Ident" },
    Symbol { id: 0x0012, name: "(" },
    Symbol { id: 0x0013, name: ")" },
    Symbol { id: 0x0014, name: "*" },
    Symbol { id: 0x0015, name: "/" },
    Symbol { id: 0x0016, name: "+" },
    Symbol { id: 0x0017, name: "-" }];


pub const VARIABLES: &[Symbol] = &[
    Symbol { id: 0x000B, name: "exp_atom" },
    Symbol { id: 0x000C, name: "exp_factor" },
    Symbol { id: 0x000D, name: "exp_term" },
    Symbol { id: 0x000E, name: "exp" },
    Symbol { id: 0x000F, name: "atom" },
    Symbol { id: 0x0010, name: "exprLess" },
    Symbol { id: 0x0011, name: "exprG" },
    Symbol { id: 0x0018, name: "__VAxiom" }];

pub const VIRTUALS: &[Symbol] = &[];