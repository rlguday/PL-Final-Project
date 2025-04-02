#include <iostream>
#include <vector>
#include <string>
#include <map>
#include <memory>
#include <cctype>
#include <unordered_set>  

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
    COMMA,        //,
    STRING,
    PRINT,
    UNKNOWN
};

enum Command {
    IGNORE_SEMICOLON,
    REQUIRE_SEMICOLON
};

struct Variable {
    string varName;
    bool hasInitializer;

    Variable(const string& name, bool initialized) 
        : varName(name), hasInitializer(initialized) {}
};

vector<vector<unique_ptr<Variable>>> declaredVariablesStack;
bool isVarDeclaredInCurrentScope(const string &varName);
bool isVariableDeclared(const string &varName);
void setVariableIsInitialized(const string &varName);

class ASTNode {
public:
    virtual ~ASTNode() = default;

    virtual void generateBytecode(int &labelCounter) const = 0;

    virtual string evaluate() { return ""; }; 

    static string toString(int value) {
        return std::to_string(value);
    }
};

class StringLiteralNode : public ASTNode {
    string value;
public:
    StringLiteralNode(string val) : value(move(val)) {}

    string evaluate() override { return value; }

    void generateBytecode(int &labelCounter) const override {
        cout << "PUSH \"" << value << "\"" << endl;
    }
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
    unique_ptr<ASTNode> initializer;

    VariableDeclarationNode(const string &name, unique_ptr<ASTNode> init = nullptr)
        : varName(name), initializer(move(init)) {}

    void generateBytecode(int &labelCounter) const override {
        cout << "DECLARE_VAR " << varName << endl;

        if (initializer) {
            initializer->generateBytecode(labelCounter);
            cout << "ASSIGN " << varName << endl;
        }
    }
};

class VariableReferenceNode : public ASTNode {
public:
    const string varName;

    explicit VariableReferenceNode(const string &name) : varName(name) {}

    void generateBytecode(int &labelCounter) const override {
        cout << "PUSH_VAR " << varName << endl;
    }

    string evaluate() override {
        if (isVariableDeclared(varName)) {
            return varName;
        }
        
        throw runtime_error("Undefined variable: " + varName);
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

    string evaluate() override {
        return value;
    }

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
        int startLabel = labelCounter++;
        int endLabel = labelCounter++;

        cout << "IF_CONDITION_START_" << startLabel << endl;
        condition->generateBytecode(labelCounter);
        
        int elseLabel = labelCounter++;
        cout << "JUMP_IF_FALSE ELSE_" << elseLabel << endl;

        for (const auto &stmt : ifBody) {
            if (stmt) stmt->generateBytecode(labelCounter);
        }
        cout << "JUMP END_IF_" << endLabel << endl;

        for (const auto &elseIfBranch : elseIfBranches) {
            cout << "ELSE_" << elseLabel << ":" << endl; 
            elseIfBranch.first->generateBytecode(labelCounter);
            int nextElseLabel = labelCounter++;
            cout << "JUMP_IF_FALSE ELSE_" << nextElseLabel << endl;

            for (const auto &stmt : elseIfBranch.second) {
                if (stmt) stmt->generateBytecode(labelCounter);
            }
            cout << "JUMP END_IF_" << endLabel << endl;
            elseLabel = nextElseLabel;
        }

        cout << "ELSE_" << elseLabel << ":" << endl;
        for (const auto &stmt : elseBody) {
            if (stmt) stmt->generateBytecode(labelCounter);
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
        int currentLabel = labelCounter++;

        initialization->generateBytecode(labelCounter);
        cout << "LOOP_START_" << currentLabel <<  ":" << endl;

        condition->generateBytecode(labelCounter);
        cout << "JUMP_IF_FALSE LOOP_END_" << currentLabel << endl;

        for (const auto &stmt : body) {
            if (stmt)
                stmt->generateBytecode(labelCounter);
        }

        increment->generateBytecode(labelCounter);
        cout << "JUMP LOOP_START_" << currentLabel << endl;
        cout << "LOOP_END_" << currentLabel <<  ":" << endl;
    }
};

class WhileLoopNode : public ASTNode {
public:
    unique_ptr<ASTNode> condition;
    vector<unique_ptr<ASTNode>> body;

    WhileLoopNode(unique_ptr<ASTNode> cond, vector<unique_ptr<ASTNode>> bodyStmts)
        : condition(move(cond)), body(move(bodyStmts)) {}

    void generateBytecode(int &labelCounter) const override {
        int currentLabel = labelCounter++;

        cout << "LOOP_START_" << currentLabel << ":" << endl;
        condition->generateBytecode(labelCounter); 
        cout << "JUMP_IF_FALSE LOOP_END_" << currentLabel << endl;

        for (const auto &stmt : body) {
            stmt->generateBytecode(labelCounter);
        }

        cout << "JUMP LOOP_START_" << currentLabel << endl;
        cout << "LOOP_END_" << currentLabel << ":" << endl;
    }
};

class PrintNode : public ASTNode {
public:
    vector<unique_ptr<ASTNode>> args;
    PrintNode(vector<unique_ptr<ASTNode>> args) : args(move(args)) {}

    string evaluate() override {
        string result;
        for (auto &arg : args) {
            result += arg->evaluate() + " ";
        }
        cout << result << endl;
        return result;
    }

    void generateBytecode(int &labelCounter) const override {
        for (const auto &arg : args) {
            arg->generateBytecode(labelCounter);
        }
        cout << "PRINT" << endl; 
    }
};

bool isVariableInitialized(const string &varName) {
    for (auto it = declaredVariablesStack.rbegin(); it != declaredVariablesStack.rend(); ++it) {
        for (const auto &var : *it) {
            if (var->varName == varName) {
                if (var->hasInitializer) {
                    return true;
                }
            }
        }
    }
    return false;
}

void setVariableIsInitialized(const string &varName) {
    for (auto it = declaredVariablesStack.rbegin(); it != declaredVariablesStack.rend(); ++it) {
        for (const auto &var : *it) {
            if (var->varName == varName) {
                var->hasInitializer = true;
                return;
            }
        }
    }
}

bool isVarDeclaredInCurrentScope(const string &varName) {
    for (const auto &var : declaredVariablesStack.back()) {
        if (var->varName == varName) {
            return true;
        }
    }
    return false;
}

bool isVariableDeclared(const string &varName) {
    for (auto it = declaredVariablesStack.rbegin(); it != declaredVariablesStack.rend(); ++it) {
        for (const auto &var : *it) {
            if (var->varName == varName) {
                return true;
            }
        }
    }
    return false;
}

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
                TokenType type = identifyKeyword(word);
                tokens.push_back({type, word});
            } else if (isdigit(source[pos])) {
                tokens.push_back({NUMBER, parseNumber()});
            } else if (source[pos] == '"' || source[pos] == '\'') { // String literal
                tokens.push_back({STRING, parseString()});
            } else if (source[pos] == '+' && pos + 1 < source.length() && source[pos + 1] == '+') { // Increment operator
                tokens.push_back({OPERATOR, "++"});
                pos += 2;
            } else if (source[pos] == '-' && pos + 1 < source.length() && source[pos + 1] == '-') { // Decrement operator
                tokens.push_back({OPERATOR, "--"});
                pos += 2;
            } else if (source[pos] == '=') { // Assignment or comparison operator
                if (pos + 1 < source.length() && source[pos + 1] == '=') {
                    tokens.push_back({COMPARISON, "=="});
                    pos += 2;
                } else {
                    tokens.push_back({ASSIGNMENT, "="});
                    pos++;
                }
            } else if (string("+-*/%").find(source[pos]) != string::npos) { // Arithmetic operators
                tokens.push_back({OPERATOR, string(1, source[pos])});
                pos++;
            } else if (string("><!=&|").find(source[pos]) != string::npos) { // Comparison or logical operators
                tokens.push_back({COMPARISON, string(1, source[pos])});
                pos++;
            } else if (source[pos] == '(') { // Parenthesis
                tokens.push_back({OPEN_PAREN, "("});
                pos++;
            } else if (source[pos] == ')') {
                tokens.push_back({CLOSE_PAREN, ")"});
                pos++;
            } else if (source[pos] == '{') { // Opening brace
                tokens.push_back({OPEN_BRACE, "{"});
                pos++;
            } else if (source[pos] == '}') { // Closing brace
                tokens.push_back({CLOSE_BRACE, "}"});
                pos++;
            } else if (source[pos] == ';') { // Semicolon
                tokens.push_back({SEMICOLON, ";"});
                pos++;
            } else if (source[pos] == ',') { // Comma
                tokens.push_back({COMMA, ","});
                pos++;
            } else {
                // Unrecognized character
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

    string parseString() {
        string result;
        char quoteChar = source[pos];
        
        if (source[pos] != '"' && source[pos] != '\'') {
            throw runtime_error("Invalid string literal: Missing opening quote.");
        }
        
        pos++;

        while (pos < source.length() && source[pos] != quoteChar) {
            if (source[pos] == '\\' && pos + 1 < source.length()) { 
                pos++;
                switch (source[pos]) {
                    case 'n': result += '\n'; break; 
                    case 't': result += '\t'; break;
                    case '\\': result += '\\'; break;
                    case '"': result += '"'; break;
                    case '\'': result += '\''; break;
                    default: 
                        throw runtime_error("Invalid escape sequence: \\" + string(1, source[pos]));
                }
            } else {
                result += source[pos];
            }
            pos++;
        }

        // Ensure we find the closing quote
        if (source[pos] != quoteChar) {
            throw runtime_error("Invalid string literal: Missing closing quote.");
        }
        pos++;  // Move past the closing quote
        
        return result;
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
        if (word == "print") return PRINT;
        return IDENTIFIER;
    }
};

class Parser {
public:
    Parser(const vector<Token> &tokens) : tokens(tokens), pos(0) {
        declaredVariablesStack.emplace_back();
    }

    vector<unique_ptr<ASTNode>> parse() {
        vector<unique_ptr<ASTNode>> ast;

        try {
            while (pos < tokens.size()) {
                ast.push_back(move(parseStatement()));
            }
        } catch (const runtime_error &e) {
            cout << e.what() << endl;
            exit(EXIT_FAILURE);
        }

        if (!errorMessages.empty()) {
            for (const string &errMsg : errorMessages) {
                cout << errMsg << endl;
            }
            cout << "Parsing stopped due to critical errors.\n";
            exit(EXIT_FAILURE);
        }

        return ast;
    }

private:
    vector<Token> tokens;
    size_t pos;
    vector<string> errorMessages;

    unique_ptr<ASTNode> parseVariableDeclaration(Command command = REQUIRE_SEMICOLON) {
        if (tokens[pos].type == IPAHAYAG) {
            pos++; // skip ipahayag

            if (tokens[pos].type == IDENTIFIER) {
                string varName = tokens[pos].value;

                if (declaredVariablesStack.empty()) {
                    throw runtime_error("Internal error: Variable declaration stack is empty.");
                }
                
                if (isVarDeclaredInCurrentScope(varName)) {
                    errorMessages.push_back("Error: Variable '" + varName + "' is already declared in the current scope.\n");
                    throw runtime_error("Parsing stopped due to variable '" + varName + "' redeclaration.");
                }

                pos++; // move after identifier

                unique_ptr<ASTNode> initializer;
                unique_ptr<Variable> newVar = make_unique<Variable>(varName, false);;

                bool hasInitializer = false;

                if (tokens[pos].type == ASSIGNMENT) {
                    pos++; // move after "="

                    if (tokens[pos].type == STRING) {
                        initializer = make_unique<StringLiteralNode>(tokens[pos].value); // Assume StringNode handles string literals
                        pos++;
                    } else {
                        initializer = parseExpression();
                    }
                    hasInitializer = true;
                    newVar->hasInitializer = hasInitializer;
                }

                if (command == REQUIRE_SEMICOLON && tokens[pos].type == SEMICOLON) {
                    pos++;
                } else if (command == REQUIRE_SEMICOLON) {
                    errorMessages.push_back("Error: Missing semicolon after variable declaration.\n");
                    throw runtime_error("Parsing stopped due to missing semicolon after assignment to variable '" + varName + "'.");
                }
                
                declaredVariablesStack.back().push_back(move(newVar));

                return make_unique<VariableDeclarationNode>(varName, move(initializer));
            } else {
                errorMessages.push_back("Error: Expected identifier after 'ipahayag'.\n");
                throw runtime_error("Parsing stopped due to missing variable name.");
            }
        }

        return nullptr;
    }

    unique_ptr<ASTNode> parseAssignment(Command command = REQUIRE_SEMICOLON) {
        string varName = tokens[pos].value;

        if (!isVariableDeclared(varName)) {
            errorMessages.push_back("Error: Variable '" + varName + "' is not declared before assignment.");
            throw runtime_error("Parsing stopped due to undeclared variable usage: '" + varName + "'.");
        }

        pos += 2;
        auto expr = parseExpression();

        setVariableIsInitialized(varName);

        if (tokens[pos].type != SEMICOLON && command == REQUIRE_SEMICOLON) {
            errorMessages.push_back("Error: Missing semicolon after assignment to variable '" + varName + "'.");
            throw runtime_error("Parsing stopped due to missing semicolon after assignment to variable '" + varName + "'.");
        }

        return make_unique<AssignmentNode>(varName, move(expr));
    }

    unique_ptr<ASTNode> parseExpression() {
        int typeFlags = 0x0; 

        auto left = parsePrimary();

        if (tokens[pos - 1].type == IDENTIFIER) {
            string varName = tokens[pos - 1].value;
            if (!isVariableDeclared(varName)) {
                errorMessages.push_back("Error: Variable '" + varName + "' is not declared before use.\n");
                throw runtime_error("Parsing stopped due to variable '" + varName + "' is not declared before use.\n");
            }

            if (!isVariableInitialized(varName)) {
                errorMessages.push_back("Error: Variable '" + varName + "' is declared but not initialized.");
                throw runtime_error("Parsing stopped due to uninitialized variable '" + varName + "' usage.");
            }
        } else if (tokens[pos - 1].type == NUMBER) {
            typeFlags |= 0x1 << 0;
        } else if (tokens[pos - 1].type == STRING) {
            typeFlags |= 0x1 << 1;
        }

        while (tokens[pos].type == OPERATOR || tokens[pos].type == COMPARISON) {
            string op = tokens[pos].value;
            pos++;
            auto right = parsePrimary();

            if (tokens[pos - 1].type == IDENTIFIER) {
                string varName = tokens[pos - 1].value;
                if (!isVariableDeclared(varName)) {
                    errorMessages.push_back("Error: Variable '" + varName + "' is not declared before use.\n");
                    throw runtime_error("Parsing stopped due to variable '" + varName + "' is not declared before use.\n");
                }

                if (!isVariableInitialized(varName)) {
                    errorMessages.push_back("Error: Variable '" + varName + "' is declared but not initialized.");
                    throw runtime_error("Parsing stopped due to uninitialized variable '" + varName + "' usage.");
                }
            } else if (tokens[pos - 1].type == NUMBER) {
                typeFlags |= 0x1 << 0;
            }
            else if (tokens[pos - 1].type == STRING) {
                typeFlags |= 0x1 << 1;
            }

            if (typeFlags == 0x3) {
                errorMessages.push_back("Error: Incompatible types\n");
                throw runtime_error("Parsing stopped due to variable incompatible types.\n");
            }

            if (tokens[pos - 2].type == COMPARISON) {
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
        } else if (tokens[pos].type == STRING) {
            return make_unique<StringLiteralNode>(tokens[pos++].value);
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
            } else {
                errorMessages.push_back("Error: Missing closing parenthesis in conditional statement.");
                throw runtime_error("Parsing stopped due to missing closing parenthesis.");
            }

            vector<unique_ptr<ASTNode>> ifBody;
            vector<unique_ptr<ASTNode>> elseBody;

            declaredVariablesStack.emplace_back();

            if (tokens[pos].type == OPEN_BRACE) {
                pos++;
                while (tokens[pos].type != CLOSE_BRACE) {
                    ifBody.push_back(parseStatement());
                }
                pos++;
            }

            declaredVariablesStack.pop_back();

            vector<pair<unique_ptr<ASTNode>, vector<unique_ptr<ASTNode>>>> elseIfBranches;
            while (tokens[pos].type == PAG_IBA_KUNG) {
                pos++;
                if (tokens[pos].type == OPEN_PAREN) {
                    pos++;
                    auto elseIfCondition = parseExpression();
                    if (tokens[pos].type == CLOSE_PAREN) {
                        pos++;
                    } else {
                        errorMessages.push_back("Error: Missing closing parenthesis in else-if condition.");
                        throw runtime_error("Parsing stopped due to missing closing parenthesis.");
                    }

                    vector<unique_ptr<ASTNode>> elseIfBody;

                    declaredVariablesStack.emplace_back();

                    if (tokens[pos].type == OPEN_BRACE) {
                        pos++;
                        while (tokens[pos].type != CLOSE_BRACE) {
                            elseIfBody.push_back(parseStatement());
                        }
                        pos++;
                    }

                    declaredVariablesStack.pop_back();

                    elseIfBranches.push_back({move(elseIfCondition), move(elseIfBody)});
                }
            }

            if (tokens[pos].type == PAG_IBA) {
                pos++;

                declaredVariablesStack.emplace_back();

                if (tokens[pos].type == OPEN_BRACE) {
                    pos++;
                    while (tokens[pos].type != CLOSE_BRACE) {
                        elseBody.push_back(parseStatement());
                    }
                    pos++;
                }

                declaredVariablesStack.pop_back();
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

    unique_ptr<ASTNode> parseStatement(Command command = REQUIRE_SEMICOLON) {
        if (tokens[pos].type == IPAHAYAG) return parseVariableDeclaration();
        else if (tokens[pos].type == IDENTIFIER && tokens[pos + 1].type == ASSIGNMENT) return parseAssignment(command);
        else if (tokens[pos].type == KAPAG) return parseConditional();
        else if (tokens[pos].type == PARA_SA) return parseForLoop();
        else if (tokens[pos].type == HABANG) return parseWhileLoop();
        else if (tokens[pos].type == PRINT) return parsePrint();
        else if (tokens[pos].type == IDENTIFIER &&
            (tokens[pos + 1].type == OPERATOR && (tokens[pos + 1].value == "++" || tokens[pos + 1].value == "--"))) {
            string varName = tokens[pos].value;

            if (!isVariableDeclared(varName)) {
                errorMessages.push_back("Error: Variable '" + varName + "' is not declared before using postfix operation.\n");
                throw runtime_error("Parsing stopped due to usage of undeclared variable '" + varName + "'.");
            }

            string op = tokens[pos + 1].value;
            pos += 2;
            if (tokens[pos].type == SEMICOLON) pos++;
            return make_unique<PostfixNode>(varName, op);
        }
        else {
            pos++;
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
        pos++;  // Skip 'para_sa' token

        if (tokens[pos].type == OPEN_PAREN) {
            pos++;  // Skip '('

            // ---- New Scope for Loop Header ----
            declaredVariablesStack.emplace_back();

            unique_ptr<ASTNode> initialization;
            if (tokens[pos].type == IPAHAYAG) {
                string varName = tokens[pos + 1].value;

                initialization = parseVariableDeclaration(IGNORE_SEMICOLON);
            } else {
                initialization = parseAssignment(IGNORE_SEMICOLON);
            }

            // Ensure semicolon after initialization
            checkSemicolon(pos, "Parsing stopped due to missing semicolon at the end of para_sa initialization.");
            pos++;

            auto condition = parseExpression(); // Parse condition

            // Ensure semicolon after condition
            checkSemicolon(pos, "Parsing stopped due to missing semicolon at the end of para_sa test condition.");
            pos++;

            // Parse expression
            unique_ptr<ASTNode> increment = parseStatement(IGNORE_SEMICOLON);

            if (tokens[pos].type != CLOSE_PAREN) {
                errorMessages.push_back("Error: Missing closing parenthesis ')' in 'para_sa' loop.");
                throw runtime_error("Parsing stopped due to missing closing parenthesis.");
            }
            pos++;

            vector<unique_ptr<ASTNode>> body;

            // New Scope for Loop Body
            if (tokens[pos].type == OPEN_BRACE) {
                pos++;  // Skip '{'

                declaredVariablesStack.emplace_back();  // Push new scope for loop body
                int i = 0;
                while (tokens[pos].type != CLOSE_BRACE) {
                    body.push_back(parseStatement());
                }

                pos++;  // Skip '}'

                declaredVariablesStack.pop_back();  // Pop loop body scope
            } else {
                errorMessages.push_back("Error: Missing '{' for the body of 'para_sa' loop.");
                throw runtime_error("Parsing stopped due to missing loop body.");
            }

            declaredVariablesStack.pop_back();  // Pop loop header scope

            auto loopNode = make_unique<ForLoopNode>();
            loopNode->initialization = move(initialization);
            loopNode->condition = move(condition);
            loopNode->increment = move(increment);
            loopNode->body = move(body);

            return loopNode;
        }

        errorMessages.push_back("Error: Missing opening parenthesis '(' in 'para_sa' loop.");
        throw runtime_error("Parsing stopped due to invalid loop syntax.");
    }

    unique_ptr<ASTNode> parsePrint() {
        pos++;
        
        if (tokens[pos].type != OPEN_PAREN) {
            errorMessages.push_back("Error: Missing opening parenthesis '(' after 'print'.");
            throw runtime_error("Parsing stopped due to missing '(' after 'print'.");
        }
        pos++;

        vector<unique_ptr<ASTNode>> arguments;

        while (tokens[pos].type != CLOSE_PAREN) {
            if (tokens[pos].type == STRING) {
                arguments.push_back(make_unique<StringLiteralNode>(tokens[pos].value));
                pos++;
            } else if (tokens[pos].type == IDENTIFIER) {
                string varName = tokens[pos].value;
                if (!isVariableDeclared(varName)) {
                    errorMessages.push_back("Error: Variable '" + varName + "' is not declared before using postfix operation.\n");
                    throw runtime_error("Parsing stopped due to usage of undeclared variable '" + varName + "'. qweqwe");
                }
                if (!isVariableInitialized(varName)) {
                    errorMessages.push_back("Error: Variable '" + varName + "' is declared but not initialized.");
                    throw runtime_error("Parsing stopped due to uninitialized variable '" + varName + "' usage.");
                }
                arguments.push_back(make_unique<VariableReferenceNode>(tokens[pos].value));
                pos++;
            } else {
                errorMessages.push_back("Error: Unexpected token in print statement.");
                throw runtime_error("Parsing stopped due to unexpected token in print statement.");
            }

            if (tokens[pos].type == COMMA) {
                pos++; 
            }
        }

        // Expect a closing parenthesis ')'
        if (tokens[pos].type != CLOSE_PAREN) {
            errorMessages.push_back("Error: Missing closing parenthesis ')' in print statement.");
            throw runtime_error("Parsing stopped due to missing ')'.");
        }
        pos++;  // Skip the ')'

        // Expect a semicolon after the print statement
        if (tokens[pos].type != SEMICOLON) {
            errorMessages.push_back("Error: Missing semicolon after print statement.");
            throw runtime_error("Parsing stopped due to missing semicolon.");
        }
        pos++;  // Skip the semicolon

        // Return the PrintNode with the parsed arguments
        return make_unique<PrintNode>(move(arguments));
    }


    void checkSemicolon(int pos, string msg = "") {
        if (tokens[pos].type != SEMICOLON) {
            errorMessages.push_back("Error: Missing semicolon at the end of the statement.");
            throw runtime_error(msg);
        }
    }
};

int main() {
    string sourceCode = R"(
        ipahayag x = 10;
        ipahayag y = x;
        ipahayag z;

        kapag (x < 10) {
            z = x + y;
        }
        pag_iba_kung(y < 100) {
            print(z);
        }
    )"; 

    Lexer lexer(sourceCode);
    auto tokens = lexer.tokenize();

    Parser parser(tokens);
    auto ast = parser.parse();

    if (!ast.empty()) {
        cout << "Generated Bytecode:\n";
        int labelCounter = 0;
        for (const auto &node : ast) {
            if (node) {
                node->generateBytecode(labelCounter);
            }
        }
    } else {
        cout << "No valid AST nodes generated.\n";
    }

    return 0;
}
