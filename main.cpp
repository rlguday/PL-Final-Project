#include <iostream>
#include <vector>
#include <string>
#include <map>
#include <memory>
#include <cctype>
using namespace std;

enum TokenType {
    IPAHAYAG,    // intention to declare a variable
    KAPAG,       // IF statement
    PAG_IBA_KUNG, // ELSE_IF statement
    PAG_IBA,      // ELSE
    PARA_SA,      // FOR LOOP
    HABANG,       // WHILE LOOP
    IDENTIFIER,   // variable names
    NUMBER,       // Numbers
    OPERATOR,     // +, -, *, /
    COMPARISON,   // >, <, >=, <=, ==, !=
    ASSIGNMENT,   // =
    OPEN_PAREN,   // (
    CLOSE_PAREN,  // )
    OPEN_BRACE,   // {
    CLOSE_BRACE,  // }
    SEMICOLON,    // ;
    UNKNOWN
};

class ASTNode {
public:
    virtual ~ASTNode() = default;

    virtual void generateBytecode(int &labelCounter) const = 0;
};

class UnaryOperationNode : public ASTNode {
public:
    string varName;
    string op;

    UnaryOperationNode(const string &name, const string &op) : varName(name), op(op) {}

    void generateBytecode(int &labelCounter) const override {
        cout << "PUSH_VAR " << varName << endl;
        if (op == "++") {
            cout << "PUSH_NUM 1" << endl;
            cout << "BINARY_OP +" << endl;
        } else if (op == "--") {
            cout << "PUSH_NUM 1" << endl;
            cout << "BINARY_OP -" << endl;
        }
        cout << "STORE_VAR " << varName << endl;
    }
};


class PostfixNode : public ASTNode {
public:
    string varName;
    string op;

    PostfixNode(const string &name, const string &op) : varName(name), op(op) {}

    void generateBytecode(int &labelCounter) const override {
        cout << "PUSH_VAR " << varName << endl;
        cout << "PUSH_NUM 1" << endl;
        cout << "BINARY_OP " << (op == "++" ? "+" : "-") << endl;
        cout << "STORE_VAR " << varName << endl;
    }
};

class VariableDeclarationNode : public ASTNode {
public:
    string varName;

    explicit VariableDeclarationNode(const string &name) : varName(name) {}

    void generateBytecode(int &labelCounter) const override {
        cout << "DECLARE_VAR " << varName << endl;
    }
};

class VariableReferenceNode : public ASTNode {
public:
    string varName;

    explicit VariableReferenceNode(const string &name) : varName(name) {}

    void generateBytecode(int &labelCounter) const override {
        cout << "PUSH_VAR " << varName << endl;
    }
};

class AssignmentNode : public ASTNode {
public:
    string varName;
    unique_ptr<ASTNode> expression;

    AssignmentNode(const string &name, unique_ptr<ASTNode> expr) : varName(name), expression(move(expr)) {}

    void generateBytecode(int &labelCounter) const override {
        expression->generateBytecode(labelCounter);
        cout << "STORE_VAR " << varName << endl;
    }
};

class BinaryExpressionNode : public ASTNode {
public:
    string op;
    unique_ptr<ASTNode> left;
    unique_ptr<ASTNode> right;

    BinaryExpressionNode(const string &op, unique_ptr<ASTNode> left, unique_ptr<ASTNode> right)
        : op(op), left(move(left)), right(move(right)) {}

    void generateBytecode(int &labelCounter) const override {
        left->generateBytecode(labelCounter);
        right->generateBytecode(labelCounter);
        cout << "BINARY_OP " << op << endl;
    }
};

class ComparisonNode : public ASTNode {
public:
    string op;
    unique_ptr<ASTNode> left;
    unique_ptr<ASTNode> right;

    ComparisonNode(const string &op, unique_ptr<ASTNode> left, unique_ptr<ASTNode> right)
        : op(op), left(move(left)), right(move(right)) {}

    void generateBytecode(int &labelCounter) const override {
        left->generateBytecode(labelCounter);
        right->generateBytecode(labelCounter);
        cout << "COMPARE_OP " << op << endl;
    }
};

class NumberNode : public ASTNode {
public:
    string value;

    explicit NumberNode(const string &value) : value(value) {}

    void generateBytecode(int &labelCounter) const override {
        cout << "PUSH_NUM " << value << endl;
    }
};

class ConditionalNode : public ASTNode {
public:
    unique_ptr<ASTNode> condition;
    vector<unique_ptr<ASTNode>> ifBody;
    vector<pair<unique_ptr<ASTNode>, vector<unique_ptr<ASTNode>>>> elseIfBranches;
    vector<unique_ptr<ASTNode>> elseBody;

    void generateBytecode(int &labelCounter) const override {
        int currentLabel = labelCounter++;
        int endLabel = labelCounter++;

        cout << "IF_CONDITION_START_" << currentLabel << endl;
        condition->generateBytecode(currentLabel);
        cout << "JUMP_IF_FALSE ELSE_IF_0" << endl;

        for (const auto &stmt : ifBody) {
            stmt->generateBytecode(currentLabel);
        }
        cout << "JUMP END_IF_" << endLabel << endl;

        int elseIfLabel = 0;
        for (const auto &elseIfBranch : elseIfBranches) {
            cout << "ELSE_IF_" << elseIfLabel << ":" << endl;
            elseIfBranch.first->generateBytecode(currentLabel);
            cout << "JUMP_IF_FALSE ELSE_IF_" << elseIfLabel + 1 << endl;

            for (const auto &stmt : elseIfBranch.second) {
                stmt->generateBytecode(currentLabel);
            }
            cout << "JUMP END_IF_" << endLabel << endl;
            elseIfLabel++;
        }

        cout << "ELSE_IF_" << elseIfLabel << ":" << endl;
        for (const auto &stmt : elseBody) {
            stmt->generateBytecode(currentLabel);
        }
        cout << "END_IF_" << endLabel << ":" << endl;
    }
};

class ForLoopNode : public ASTNode {
public:
    unique_ptr<ASTNode> initialization;
    unique_ptr<ASTNode> condition;
    unique_ptr<ASTNode> increment;
    vector<unique_ptr<ASTNode>> body;

    void generateBytecode(int &labelCounter) const override {
        initialization->generateBytecode(labelCounter);
        cout << "LOOP_START:" << endl;

        condition->generateBytecode(labelCounter);
        cout << "JUMP_IF_FALSE LOOP_END" << endl;

        for (const auto &stmt : body) {
            stmt->generateBytecode(labelCounter);
        }

        increment->generateBytecode(labelCounter);
        cout << "JUMP LOOP_START" << endl;
        cout << "LOOP_END:" << endl;
    }
};

class WhileLoopNode : public ASTNode {
public:
    unique_ptr<ASTNode> condition;
    vector<unique_ptr<ASTNode>> body;

    WhileLoopNode(unique_ptr<ASTNode> cond, vector<unique_ptr<ASTNode>> bodyStmts)
        : condition(move(cond)), body(move(bodyStmts)) {}

    void generateBytecode(int &labelCounter) const override {
        cout << "LOOP_START:" << endl;
        condition->generateBytecode(labelCounter); 
        cout << "JUMP_IF_FALSE LOOP_END" << endl;

        for (const auto &stmt : body) {
            stmt->generateBytecode(labelCounter);
        }

        cout << "JUMP LOOP_START" << endl;
        cout << "LOOP_END:" << endl;
    }
};

struct Token {
    TokenType type;
    string value;
};

class Lexer {
public:
    Lexer(const string &source) : source(source), pos(0) {}

    vector<Token> tokenize() {
        vector<Token> tokens;
        while (pos < source.length()) {
            if (isspace(source[pos])) {
                pos++;
                continue;
            }

            if (isalpha(source[pos])) {
                string word = parseWord();
                tokens.push_back({identifyKeyword(word), word});
            } else if (isdigit(source[pos])) {
                tokens.push_back({NUMBER, parseNumber()});
            } else if (source[pos] == '+' && pos + 1 < source.length() && source[pos + 1] == '+') {
                tokens.push_back({OPERATOR, "++"});
                pos += 2;
            } else if (source[pos] == '-' && pos + 1 < source.length() && source[pos + 1] == '-') {
                tokens.push_back({OPERATOR, "--"});
                pos += 2;
            } else if (source[pos] == '=') {
                if (pos + 1 < source.length() && source[pos + 1] == '=') {
                    tokens.push_back({COMPARISON, "=="});
                    pos += 2;
                } else {
                    tokens.push_back({ASSIGNMENT, "="});
                    pos++;
                }
            } else if (string("+-*/").find(source[pos]) != string::npos) {
                tokens.push_back({OPERATOR, string(1, source[pos])});
                pos++;
            } else if (string("><!=").find(source[pos]) != string::npos) {
                tokens.push_back({COMPARISON, string(1, source[pos])});
                pos++;
            } else if (source[pos] == '(') {
                tokens.push_back({OPEN_PAREN, "("});
                pos++;
            } else if (source[pos] == ')') {
                tokens.push_back({CLOSE_PAREN, ")"});
                pos++;
            } else if (source[pos] == '{') {
                tokens.push_back({OPEN_BRACE, "{"});
                pos++;
            } else if (source[pos] == '}') {
                tokens.push_back({CLOSE_BRACE, "}"});
                pos++;
            } else if (source[pos] == ';') {
                tokens.push_back({SEMICOLON, ";"});
                pos++;
            } else {
                tokens.push_back({UNKNOWN, string(1, source[pos])});
                pos++;
            }
        }
        return tokens;
    }


private:
    string source;
    size_t pos;

    string parseWord() {
        string word;
        while (pos < source.length() && (isalnum(source[pos]) || source[pos] == '_')) {
            word += source[pos];
            pos++;
        }
        return word;
    }

    string parseNumber() {
        string num;
        while (pos < source.length() && isdigit(source[pos])) {
            num += source[pos];
            pos++;
        }
        return num;
    }

    TokenType identifyKeyword(const string &word) {
        if (word == "ipahayag") return IPAHAYAG;
        if (word == "kapag") return KAPAG;
        if (word == "pag_iba_kung") return PAG_IBA_KUNG;
        if (word == "pag_iba") return PAG_IBA;
        if (word == "para_sa") return PARA_SA;
        if (word == "habang") return HABANG;
        return IDENTIFIER;
    }
};

class Parser {
public:
    Parser(const vector<Token> &tokens) : tokens(tokens), pos(0) {}

    vector<unique_ptr<ASTNode>> parse() {
        vector<unique_ptr<ASTNode>> ast;

        while (pos < tokens.size()) {
            Token token = tokens[pos];
            if (token.type == IPAHAYAG) {
                ast.push_back(parseVariableDeclaration());
            } else if (token.type == IDENTIFIER && tokens[pos + 1].type == ASSIGNMENT) {
                ast.push_back(parseAssignment());
            } else if (token.type == KAPAG) {
                ast.push_back(parseConditional());
            } else if (token.type == PARA_SA) {
                ast.push_back(parseForLoop());
            } else if (token.type == HABANG) {
                ast.push_back(parseWhileLoop());
            } else {
                pos++;
            }
        }
        return ast;
    }

private:
    vector<Token> tokens;
    size_t pos;

    unique_ptr<ASTNode> parseVariableDeclaration() {
        pos++;
        if (tokens[pos].type == IDENTIFIER) {
            string varName = tokens[pos].value;
            pos++;
            if (tokens[pos].type == ASSIGNMENT) {
                pos++;
                auto expr = parseExpression();
                if (tokens[pos].type == SEMICOLON) pos++;
                return make_unique<AssignmentNode>(varName, move(expr));
            }
            if (tokens[pos].type == SEMICOLON) pos++;
            return make_unique<VariableDeclarationNode>(varName);
        }
        return nullptr;
    }

    unique_ptr<ASTNode> parseAssignment() {
        string varName = tokens[pos].value;
        pos += 2;
        auto expr = parseExpression();
        if (tokens[pos].type == SEMICOLON) pos++;
        return make_unique<AssignmentNode>(varName, move(expr));
    }

    unique_ptr<ASTNode> parseExpression() {
        auto left = parsePrimary();

        while (tokens[pos].type == OPERATOR || tokens[pos].type == COMPARISON) {
            string op = tokens[pos].value;
            pos++;
            auto right = parsePrimary();
            if (tokens[pos - 1].type == COMPARISON) {
                left = make_unique<ComparisonNode>(op, move(left), move(right));
            } else {
                left = make_unique<BinaryExpressionNode>(op, move(left), move(right));
            }
        }
        return left;
    }

    unique_ptr<ASTNode> parsePrimary() {
        if (tokens[pos].type == NUMBER) {
            return make_unique<NumberNode>(tokens[pos++].value);
        } else if (tokens[pos].type == IDENTIFIER) {
            return make_unique<VariableReferenceNode>(tokens[pos++].value);
        }
        return nullptr;
    }

    unique_ptr<ASTNode> parseConditional() {
        pos++;

        if (tokens[pos].type == OPEN_PAREN) {
            pos++;
            auto condition = parseExpression();
            if (tokens[pos].type == CLOSE_PAREN) {
                pos++;
            }

            vector<unique_ptr<ASTNode>> ifBody;
            vector<unique_ptr<ASTNode>> elseBody;

            if (tokens[pos].type == OPEN_BRACE) {
                pos++;
                while (tokens[pos].type != CLOSE_BRACE) {
                    ifBody.push_back(parseStatement());
                }
                pos++;
            }

            vector<pair<unique_ptr<ASTNode>, vector<unique_ptr<ASTNode>>>> elseIfBranches;
            while (tokens[pos].type == PAG_IBA_KUNG) {
                pos++;

                if (tokens[pos].type == OPEN_PAREN) {
                    pos++;
                    auto elseIfCondition = parseExpression();
                    if (tokens[pos].type == CLOSE_PAREN) {
                        pos++;
                    }

                    vector<unique_ptr<ASTNode>> elseIfBody;
                    if (tokens[pos].type == OPEN_BRACE) {
                        pos++;
                        while (tokens[pos].type != CLOSE_BRACE) {
                            elseIfBody.push_back(parseStatement());
                        }
                        pos++;
                    }

                    elseIfBranches.push_back({move(elseIfCondition), move(elseIfBody)});
                }
            }

            if (tokens[pos].type == PAG_IBA) {
                pos++;
                if (tokens[pos].type == OPEN_BRACE) {
                    pos++;
                    while (tokens[pos].type != CLOSE_BRACE) {
                        elseBody.push_back(parseStatement());
                    }
                    pos++;
                }
            }

            auto node = make_unique<ConditionalNode>();
            node->condition = move(condition);
            node->ifBody = move(ifBody);
            node->elseIfBranches = move(elseIfBranches);
            node->elseBody = move(elseBody);
            return node;
        }
        return nullptr;
    }

    unique_ptr<ASTNode> parseStatement() {
        if (tokens[pos].type == IPAHAYAG) return parseVariableDeclaration();
        if (tokens[pos].type == IDENTIFIER && tokens[pos + 1].type == ASSIGNMENT) return parseAssignment();
        if (tokens[pos].type == KAPAG) return parseConditional();
        if (tokens[pos].type == PARA_SA) return parseForLoop();
        if (tokens[pos].type == HABANG) return parseWhileLoop();
        if (tokens[pos].type == IDENTIFIER && 
            (tokens[pos + 1].type == OPERATOR && (tokens[pos + 1].value == "++" || tokens[pos + 1].value == "--"))) {
            string varName = tokens[pos].value;
            string op = tokens[pos + 1].value; 
            pos += 2;
            if (tokens[pos].type == SEMICOLON) pos++;
            return make_unique<PostfixNode>(varName, op);
        }

        return nullptr;
    }

    unique_ptr<ASTNode> parseWhileLoop() {
        pos++; 
        if (tokens[pos].type == OPEN_PAREN) {
            pos++; 

            auto condition = parseExpression();
            if (tokens[pos].type == CLOSE_PAREN) {
                pos++;
            }

            vector<unique_ptr<ASTNode>> body;

            if (tokens[pos].type == OPEN_BRACE) {
                pos++;
                while (tokens[pos].type != CLOSE_BRACE) {
                    body.push_back(parseStatement()); 
                }
                pos++;
            }

            return make_unique<WhileLoopNode>(move(condition), move(body));
        }
        return nullptr;
    }

    unique_ptr<ASTNode> parseForLoop() {
        pos++;

        if (tokens[pos].type == OPEN_PAREN) {
            pos++; 

            auto initialization = parseAssignment();  

            auto condition = parseExpression();  
            if (tokens[pos].type == SEMICOLON) pos++;

            unique_ptr<ASTNode> increment;
            if (tokens[pos].type == IDENTIFIER && (tokens[pos + 1].value == "++" || tokens[pos + 1].value == "--")) {
                string varName = tokens[pos].value;
                string op = tokens[pos + 1].value;
                pos += 2; 
                increment = make_unique<UnaryOperationNode>(varName, op);
            } else {
                increment = parseAssignment();
            }

            if (tokens[pos].type == CLOSE_PAREN) pos++;  

            vector<unique_ptr<ASTNode>> body;

            if (tokens[pos].type == OPEN_BRACE) {
                pos++;  
                while (tokens[pos].type != CLOSE_BRACE) {
                    body.push_back(parseStatement());
                }
                pos++;
            }

            auto loopNode = make_unique<ForLoopNode>();
            loopNode->initialization = move(initialization);
            loopNode->condition = move(condition);
            loopNode->increment = move(increment);
            loopNode->body = move(body);
            return loopNode;
        }
        return nullptr;
    }

};

int main() {
    string sourceCode = R"(
        ipahayag x = 10;

        kapag (x > 10) {
            y = y + 1;
        } 
        pag_iba_kung (x == 5) {
            y = y - 1;
        } 
        pag_iba {
            y = 0;
        }
    )";

    // kapag (x > 10) {
    //     kapag (y > 5) {
    //         y = y - 1;
    //     }
    //     pag_iba_kung (y == 5) {
    //         y = y + 1;
    //     }
    //     pag_iba {
    //         y = 0;
    //     }
    // } 
    // pag_iba_kung (x == 10) {
    //     kapag (z < 0) {
    //         z = z * 2;
    //     }
    // }
    // pag_iba {
    //     x = 0;
    // }

    // kapag (x > 10) {
    //     y = y + 1;
    // } 
    // pag_iba_kung (x == 5) {
    //     y = y - 1;
    // } 
    // pag_iba {
    //     y = 0;
    // }

    // kapag (x > 0) {
    //     x = x - 1;
    // }

    // para_sa (i = 0; i < 10; i = i + 1) {
    //     x = x + 1;
    // }

    Lexer lexer(sourceCode);
    auto tokens = lexer.tokenize();

    Parser parser(tokens);
    auto ast = parser.parse();

    cout << "Generated Bytecode:\n";
    int labelCounter = 0;
    for (const auto &node : ast) {
        node->generateBytecode(labelCounter);
    }

    return 0;
}
