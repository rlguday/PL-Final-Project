#include <iostream>
#include <vector>
#include <string>
#include <map>
#include <memory>
#include <cctype>
#include <unordered_set>  

using namespace std;

enum TokenType {
    IPAHAYAG = 5,    // intention to declare a variable
    KAPAG = 20,       // IF statement
    PAG_IBA_KUNG = 35, // ELSE_IF statement
    PAG_IBA = 40,      // ELSE
    PARA_SA = 43,      // FOR LOOP
    HABANG = 47,       // WHILE LOOP
    IDENTIFIER = 100,   // variable names
    NUMBER = 144,       // Numbers
    OPERATOR = 223,     // +, -, *, /
    COMPARISON = 234,   // >, <, >=, <=, ==, !=
    ASSIGNMENT = 346,   // =
    OPEN_PAREN,   // (
    CLOSE_PAREN,  // )
    OPEN_BRACE,   // {
    CLOSE_BRACE,  // }
    SEMICOLON,    // ;
    COMMA,        //,
    STRING,
    PRINT,
    UNKNOWN,
    E_O_F
};

enum Command {
    IGNORE_SEMICOLON,
    REQUIRE_SEMICOLON
};

struct Variable {
    string varName;
    bool hasInitializer;
    TokenType type;

    Variable(const string& name, bool initialized, TokenType type) 
        : varName(name), hasInitializer(initialized), type(type) {}
};

vector<vector<unique_ptr<Variable>>> declaredVariablesStack;
bool isVarDeclaredInCurrentScope(const string &varName);
bool isVariableDeclared(const string &varName);
void setVariableIsInitialized(const string &varName);
Variable* retrieveVariable(const string &varName);

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

        if(initializer) {
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
            stmt->generateBytecode(labelCounter);
        }
        cout << "JUMP END_IF_" << endLabel << endl;

        for (const auto &elseIfBranch : elseIfBranches) {
            cout << "ELSE_" << elseLabel << ":" << endl; 
            elseIfBranch.first->generateBytecode(labelCounter);
            int nextElseLabel = labelCounter++;
            cout << "JUMP_IF_FALSE ELSE_" << nextElseLabel << endl;

            for (const auto &stmt : elseIfBranch.second) {
                stmt->generateBytecode(labelCounter);
            }
            cout << "JUMP END_IF_" << endLabel << endl;
            elseLabel = nextElseLabel;
        }

        cout << "ELSE_" << elseLabel << ":" << endl;
        for (const auto &stmt : elseBody) {
            stmt->generateBytecode(labelCounter);
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
            if (stmt)
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

Variable* retrieveVariable(const string &varName) {
    for (auto it = declaredVariablesStack.rbegin(); it != declaredVariablesStack.rend(); ++it) {
        for (const auto &var : *it) {
            if (var->varName == varName) {
                return var.get();
            }
        }
    }
    return nullptr;
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
                if (source[pos] == '<') {
                    if (pos + 1 < source.length() && source[pos + 1] == '=') {
                        tokens.push_back({COMPARISON, "<="});
                        pos += 2;

                        continue;
                    }
                } else if (source[pos] == '>') {
                    if (pos + 1 < source.length() && source[pos + 1] == '=') {
                        tokens.push_back({COMPARISON, ">="});
                        pos += 2;

                        continue;
                    }
                } else if (source[pos] == '!') {
                    if (pos + 1 < source.length() && source[pos + 1] == '=') {
                        tokens.push_back({COMPARISON, "!="});
                        pos += 2;

                        continue;
                    }
                }

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
        tokens.push_back({E_O_F, string(1, ' ')});
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

        string syntaxRequirement = "";
        try {
            while (tokens[pos].type != E_O_F) {
                unique_ptr<ASTNode> statement = parseStatement(syntaxRequirement);

                ast.push_back(move(statement));
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

    int movePositionBy(int incrementAmount) {
        int tmp = pos + incrementAmount;
        if (tmp >= tokens.size()) {
            pos = tokens.size() - 1;
        }
        else {
            pos += incrementAmount;
        }

        return pos;
    }

    unique_ptr<ASTNode> parseVariableDeclaration(string& syntaxRequirementOut, Command command = REQUIRE_SEMICOLON) {
        if (tokens[pos].type == IPAHAYAG) {
            movePositionBy(1); // skip ipahayag

            if (tokens[pos].type == IDENTIFIER) {
                string varName = tokens[pos].value;

                if (declaredVariablesStack.empty()) {
                    throw runtime_error("Internal error: Variable declaration stack is empty.");
                }
                
                if (isVarDeclaredInCurrentScope(varName)) {
                    syntaxRequirementOut = "| REASON: variable '" + varName + "' is already declared.";
                    return nullptr;
                }

                movePositionBy(1); // move after identifier

                unique_ptr<ASTNode> initializer;
                unique_ptr<Variable> newVar = make_unique<Variable>(varName, false, UNKNOWN);

                if (tokens[pos].type == ASSIGNMENT) {
                    movePositionBy(1); // move after "="

                    initializer = parseExpression(syntaxRequirementOut, newVar.get());

                    if (!initializer) {
                        return nullptr;
                    }

                    newVar->hasInitializer = true;
                }

                if (command == REQUIRE_SEMICOLON && tokens[pos].type == SEMICOLON) {
                    movePositionBy(1);;
                } else if (command == REQUIRE_SEMICOLON) {
                    syntaxRequirementOut = "| REASON: requires ';'";
                    return nullptr;
                }
                
                declaredVariablesStack.back().push_back(move(newVar));

                return make_unique<VariableDeclarationNode>(varName, move(initializer));
            } else {
                syntaxRequirementOut = "| REASON: requires variable name";
            }
        }

        return nullptr;
    }

    unique_ptr<ASTNode> parseAssignment(string& syntaxRequirementOut, Command command = REQUIRE_SEMICOLON) {
        string varName = tokens[pos].value;

        if (!isVariableDeclared(varName)) {
            syntaxRequirementOut = "| REASON: requires declaration of variable '" + varName + "'.";
            return nullptr;
        }

        Variable *containerVar = retrieveVariable(varName);

        movePositionBy(2);;
        auto expr = parseExpression(syntaxRequirementOut, nullptr, containerVar);

        if (!expr) {
            return nullptr;
        }

        setVariableIsInitialized(varName);

        if (tokens[pos].type != SEMICOLON && command == REQUIRE_SEMICOLON) {
            syntaxRequirementOut = "| REASON: requires semicolon after assignment to variable '" + varName + "'.";
            return nullptr;
        }
        else if (command == REQUIRE_SEMICOLON) {
            movePositionBy(1);
        }

        return make_unique<AssignmentNode>(varName, move(expr));
    }

    unique_ptr<ASTNode> parseExpression(string& syntaxRequirementOut, Variable * newVar = nullptr, Variable *containerVar = nullptr) {
        int typeFlags = 0x0; 

        if (containerVar  && (containerVar->type != UNKNOWN)) {
            typeFlags |= containerVar->type;
        }

        auto left = parsePrimary();

        if (!left) {
            return nullptr;
        }

        if (tokens[pos - 1].type == IDENTIFIER) {
            string varName = tokens[pos - 1].value;

            if (!isVariableDeclared(varName)) {
                syntaxRequirementOut = "| REASON: requires declaration of variable '" + varName + "'.";
                return nullptr;
            }

            if (!isVariableInitialized(varName)) {
                syntaxRequirementOut = "| REASON: requires initialization of variable '" + varName + "'.";
                return nullptr;
            }

            Variable* rVar = retrieveVariable(varName);

            if (rVar && (rVar->type != UNKNOWN)) {
                typeFlags |= rVar->type;
            }
            else {
                syntaxRequirementOut = "| REASON: requires initialization of variable '" + varName + "'.";
                return nullptr;
            }
        } else if (tokens[pos - 1].type == NUMBER) {
            typeFlags |= NUMBER;
        } else if (tokens[pos - 1].type == STRING) {
            typeFlags |= STRING;
        }

        while (tokens[pos].type == OPERATOR || tokens[pos].type == COMPARISON) {
            string op = tokens[pos].value;

            movePositionBy(1);

            auto right = parsePrimary();

            if (!right) {
                return nullptr;
            }

            if (tokens[pos - 1].type == IDENTIFIER) {
                string varName = tokens[pos - 1].value;
                if (!isVariableDeclared(varName)) {
                    syntaxRequirementOut = "| REASON: requires declaration of variable '" + varName + "'.";
                    return nullptr;
                }

                if (!isVariableInitialized(varName)) {
                    syntaxRequirementOut = "| REASON: requires initialization of variable '" + varName + "'.";
                    return nullptr;
                }

                Variable* rVar = retrieveVariable(varName);
                if (rVar && (rVar->type != UNKNOWN)) {
                    typeFlags |= rVar->type;
                }
                else {
                    syntaxRequirementOut = "| REASON: requires initialization of variable '" + varName + "'.";
                    return nullptr;
                }
            } else if (tokens[pos - 1].type == NUMBER) {
                typeFlags |= NUMBER;
            }
            else if (tokens[pos - 1].type == STRING) {
                typeFlags |= STRING;
            }

            if (typeFlags == (NUMBER|STRING)) {
                syntaxRequirementOut = "| REASON: variable incompatible types.";
                return nullptr;
            }

            if (tokens[pos - 2].type == COMPARISON) {
                left = make_unique<ComparisonNode>(op, move(left), move(right));
            } else {
                left = make_unique<BinaryExpressionNode>(op, move(left), move(right));
            }
        }

        if (!(typeFlags == (NUMBER|STRING))) {
            if (newVar) {
                newVar->type = TokenType(typeFlags);
            } 
            else if (containerVar  && (containerVar->type == UNKNOWN)) {
                containerVar->type = TokenType(typeFlags);
            }
        } else {
            syntaxRequirementOut = "| REASON: variable '" + containerVar->varName +"' incompatible types.";
            return nullptr;
        }

        if (!left) {
            syntaxRequirementOut = "nigarti";
            return nullptr;
        }

        return left;
    }

    unique_ptr<ASTNode> parsePrimary() {
        if (tokens[pos].type == NUMBER) {
            unique_ptr<NumberNode> numNode = make_unique<NumberNode>(tokens[pos].value);
            movePositionBy(1);
            return numNode;
        } else if (tokens[pos].type == STRING) {
            unique_ptr<StringLiteralNode> stringLiteralNode = make_unique<StringLiteralNode>(tokens[pos].value);
            movePositionBy(1);
            return stringLiteralNode;
        } else if (tokens[pos].type == IDENTIFIER) {
            unique_ptr<VariableReferenceNode> varRefNode = make_unique<VariableReferenceNode>(tokens[pos].value);
            movePositionBy(1);
            return varRefNode;
        }
        return nullptr;
    }

    unique_ptr<ASTNode> parseConditional(string& syntaxRequirementOut) {
        movePositionBy(1);

        if (tokens[pos].type == OPEN_PAREN) {
            movePositionBy(1);

            auto condition = parseExpression(syntaxRequirementOut);
            if (!condition) {
                return nullptr;
            }

            if (tokens[pos].type == CLOSE_PAREN) {
                movePositionBy(1);
            } else {
                syntaxRequirementOut = "| REASON: requires ')'";
                return nullptr;
            }

            vector<unique_ptr<ASTNode>> ifBody;
            vector<unique_ptr<ASTNode>> elseBody;

            declaredVariablesStack.emplace_back();

            if (tokens[pos].type == OPEN_BRACE) {
                movePositionBy(1);

                while (tokens[pos].type != CLOSE_BRACE) {
                    ifBody.push_back(parseStatement(syntaxRequirementOut));
                }

                if (tokens[pos].type == CLOSE_BRACE) {
                    movePositionBy(1); 
                }
                else {
                    return nullptr;
                }
            } else {
                syntaxRequirementOut = "| REASON: requires '{'";
                return nullptr;
            }

            declaredVariablesStack.pop_back();

            vector<pair<unique_ptr<ASTNode>, vector<unique_ptr<ASTNode>>>> elseIfBranches;
            while (tokens[pos].type == PAG_IBA_KUNG) {
                movePositionBy(1);
                if (tokens[pos].type == OPEN_PAREN) {
                    movePositionBy(1);

                    auto elseIfCondition = parseExpression(syntaxRequirementOut);

                    if (!elseIfCondition) {
                        return nullptr;
                    }

                    if (tokens[pos].type == CLOSE_PAREN) {
                        movePositionBy(1);
                    } else {
                        syntaxRequirementOut = "| REASON: requires ')'";
                        return nullptr;
                    }

                    vector<unique_ptr<ASTNode>> elseIfBody;

                    declaredVariablesStack.emplace_back();

                    if (tokens[pos].type == OPEN_BRACE) {
                        movePositionBy(1);

                        while (tokens[pos].type != CLOSE_BRACE) {
                            elseIfBody.push_back(parseStatement(syntaxRequirementOut));
                        }
                        if (tokens[pos].type == CLOSE_BRACE) {
                            movePositionBy(1);   
                        }
                        else {
                            return nullptr;
                        }
                    } else {
                        syntaxRequirementOut = "| REASON: requires '{'";
                        return nullptr;
                    }

                    declaredVariablesStack.pop_back();

                    elseIfBranches.push_back({move(elseIfCondition), move(elseIfBody)});
                } else {
                    syntaxRequirementOut = "| REASON: requires '('";
                    return nullptr;
                }
            }

            if (tokens[pos].type == PAG_IBA) {
                movePositionBy(1);

                declaredVariablesStack.emplace_back();

                if (tokens[pos].type == OPEN_BRACE) {
                    movePositionBy(1);
                    
                    while (tokens[pos].type != CLOSE_BRACE) {
                        elseBody.push_back(parseStatement(syntaxRequirementOut));
                    }
                    if (tokens[pos].type == CLOSE_BRACE) {
                        movePositionBy(1); 
                    }
                    else {
                        return nullptr;
                    }
                } else {
                    syntaxRequirementOut = "| REASON: requires '{'";
                    return nullptr;
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
        else {
            syntaxRequirementOut = "| REASON: requires '('";
            return nullptr;
        }
    }

    unique_ptr<ASTNode> parseStatement(string& syntaxRequirementOut, Command command = REQUIRE_SEMICOLON) {
        unique_ptr<ASTNode> statement = nullptr;

        if (tokens[pos].type == IPAHAYAG) { statement = parseVariableDeclaration(syntaxRequirementOut);}
        else if (tokens[pos].type == IDENTIFIER && tokens[pos + 1].type == ASSIGNMENT) 
            { statement = parseAssignment(syntaxRequirementOut, command); }
        else if (tokens[pos].type == KAPAG) { statement = parseConditional(syntaxRequirementOut); }
        else if (tokens[pos].type == PARA_SA) { statement = parseForLoop(syntaxRequirementOut); }
        else if (tokens[pos].type == HABANG) { statement = parseWhileLoop(syntaxRequirementOut); }
        else if (tokens[pos].type == PRINT) { statement = parsePrint(syntaxRequirementOut); }
        else if (tokens[pos].type == IDENTIFIER &&
            (tokens[pos + 1].type == OPERATOR && (tokens[pos + 1].value == "++" || tokens[pos + 1].value == "--"))) {
            string varName = tokens[pos].value;

            if (!isVariableDeclared(varName)) {
                syntaxRequirementOut = "| REASON: requires declaration of variable '" + varName + "'.";
                return nullptr;
            }

            string op = tokens[pos + 1].value;
            movePositionBy(2);

            if (command == REQUIRE_SEMICOLON && tokens[pos].type == SEMICOLON) {
                movePositionBy(1);
            } 
            else if (command == REQUIRE_SEMICOLON) {
                syntaxRequirementOut = "| REASON: requires ';'";
            }
            else {
                statement = make_unique<PostfixNode>(varName, op);
            }
        }

        if (!statement) {
            throw runtime_error(
                "Parsing stopped due to " +
                (syntaxRequirementOut != "" ? syntaxRequirementOut : "") +
                "| unexpected token '" + tokens[pos].value + "' after '" + tokens[pos-1].value + "'");
        }

        return statement;
    }

    unique_ptr<ASTNode> parseWhileLoop(string& syntaxRequirementOut) {
        movePositionBy(1); 
        if (tokens[pos].type == OPEN_PAREN) {
            movePositionBy(1);

            auto condition = parseExpression(syntaxRequirementOut);

            if (!condition) {
                return nullptr;
            }

            if (tokens[pos].type == CLOSE_PAREN) {
                movePositionBy(1);
            }

            vector<unique_ptr<ASTNode>> body;

            if (tokens[pos].type == OPEN_BRACE) {
                declaredVariablesStack.emplace_back();

                movePositionBy(1);
                while (tokens[pos].type != CLOSE_BRACE) {
                    body.push_back(parseStatement(syntaxRequirementOut)); 
                }
                movePositionBy(1);
            }

            return make_unique<WhileLoopNode>(move(condition), move(body));
        }
        return nullptr;
    }

    unique_ptr<ASTNode> parseForLoop(string& syntaxRequirementOut) {
        movePositionBy(1);  // Skip 'para_sa' token

        if (tokens[pos].type == OPEN_PAREN) {
            movePositionBy(1);  // Skip '('

            // ---- New Scope for Loop Header ----
            declaredVariablesStack.emplace_back();

            unique_ptr<ASTNode> initialization;
            if (tokens[pos].type == IPAHAYAG) {
                string varName = tokens[pos + 1].value;

                initialization = parseVariableDeclaration(syntaxRequirementOut, IGNORE_SEMICOLON);
            } else {
                initialization = parseAssignment(syntaxRequirementOut, IGNORE_SEMICOLON);
            }

            // Ensure semicolon after initialization
            if (tokens[pos].type != SEMICOLON) {
                syntaxRequirementOut = "| REASON: requires ';'";
                return nullptr;
            }
            movePositionBy(1);

            auto condition = parseExpression(syntaxRequirementOut); // Parse condition

            // Ensure semicolon after condition
            if (tokens[pos].type != SEMICOLON) {
                syntaxRequirementOut = "| REASON: requires ';'";
                return nullptr;
            }
            movePositionBy(1);

            // Parse expression
            unique_ptr<ASTNode> increment = parseStatement(syntaxRequirementOut, IGNORE_SEMICOLON);

            if (tokens[pos].type != CLOSE_PAREN) {
                syntaxRequirementOut = "| REASON: requires ')'";
                return nullptr;
            }
            movePositionBy(1);

            vector<unique_ptr<ASTNode>> body;

            // New Scope for Loop Body
            if (tokens[pos].type == OPEN_BRACE) {
                movePositionBy(1);  // Skip '{'

                declaredVariablesStack.emplace_back();  // Push new scope for loop body
                int i = 0;
                while (tokens[pos].type != CLOSE_BRACE) {
                    body.push_back(parseStatement(syntaxRequirementOut));
                }

                movePositionBy(1);  // Skip '}'

                declaredVariablesStack.pop_back();  // Pop loop body scope
            } else {
                syntaxRequirementOut = "| REASON: requires '{'";
                return nullptr;
            }

            declaredVariablesStack.pop_back();  // Pop loop header scope

            if (!initialization || !condition || !increment) {
                return nullptr;
            }

            auto loopNode = make_unique<ForLoopNode>();
            loopNode->initialization = move(initialization);
            loopNode->condition = move(condition);
            loopNode->increment = move(increment);
            loopNode->body = move(body);

            return loopNode;
        } else {
            syntaxRequirementOut = "| REASON: requires '('";
            return nullptr;
        }

    }

    unique_ptr<ASTNode> parsePrint(string& syntaxRequirementOut) {
        movePositionBy(1);
        
        if (tokens[pos].type != OPEN_PAREN) {
            syntaxRequirementOut = "| REASON: requires '('";
            return nullptr;
        }
        movePositionBy(1);

        vector<unique_ptr<ASTNode>> arguments;

        while (tokens[pos].type != CLOSE_PAREN) {
            unique_ptr<ASTNode> expr = parseExpression(syntaxRequirementOut);

            if (!expr) {
                return nullptr;
            }

            arguments.push_back(move(expr));

            if (tokens[pos].type == COMMA) {
                movePositionBy(1); 
            }
            else {
                return nullptr;
            }
        }

        // Expect a closing parenthesis ')'
        if (tokens[pos].type != CLOSE_PAREN) {
            syntaxRequirementOut = "| REASON: requires ')'";
            return nullptr;
        }
        movePositionBy(1);  // Skip the ')'

        // Expect a semicolon after the print statement
        if (tokens[pos].type != SEMICOLON) {
            syntaxRequirementOut = "| REASON: requires ';'";
            return nullptr;
        }
        movePositionBy(1);  // Skip the semicolon

        // Return the PrintNode with the parsed arguments
        return make_unique<PrintNode>(move(arguments));
    }

};

int main() {
    string sourceCode = R"(
        ipahayag a = 5;
        ipahayag b = 10;
        print("Sum: ", a + b);
    )"; 

    Lexer lexer(sourceCode);
    auto tokens = lexer.tokenize();

    Parser parser(tokens);
    auto ast = parser.parse();

    if (!ast.empty()) {
        cout << "Generated Bytecode:\n";
        int labelCounter = 0;
        for (const auto &node : ast) {
            node->generateBytecode(labelCounter);
        }
    } else {
        cout << "No valid AST nodes generated.\n";
    }

    return 0;
}
