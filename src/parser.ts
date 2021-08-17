import { ElseStatement, NullExpression, ArrayExpression, AssignableExpression, Assignment, BinaryExpression, CallAccess, CallStatement, CodeBlock, Expression, FieldAccess, IfStatement, InvalidExpression, LiteralExpression, MainBlock, RootNode, Statement, UnaryExpression, VarDeclaration, VariableAccess, ModifierList, ParameterNode, FunctionNode, ReturnStatement, ExpressionList, StringExpression, ClassDeclaration, ClassMember, MethodNode, FieldNode, ConstructorParamNode, ConstructorNode, ThisAccess, ASTNode, IndexAccess, TypeParameterNode, RepeatStatement, ForStatement } from "./ast";
import { BasicTypeToken, ComboTypeToken, FunctionTypeToken, SingleTypeToken, TypeToken } from "./oop";
import { AssignmentOperator, BinaryOperator, Modifier } from "./operators";
import { DelegateTokenizer, ExpressionSegment, LiteralSegment, Position, StringToken, TextRange, Token, Tokenizer, TokenType } from "./tokenizer";

export class Parser {

    lookforward: Token[] = []
    lastToken: Token
    hasErrors = false

    constructor(public file: string, public tokenizer: Tokenizer) {
        this.lastToken = Token.EOF;
    }

    error(token: Token, msg: string) {
        this.errorRange(token.range, msg + ', found ' + token.value)
    }

    errorRange(range: TextRange, msg: string) {
        console.trace('Compilation error at ' + TextRange.toString(range) + ': ' + msg)
        this.hasErrors = true
    }

    /**
     * Returns the next token from the tokenizer
     * @param skipComments true by default. False to not skip any comments in code
     */
    next(skipComments = true): Token {
        if (this.lookforward.length > 0) {
            let n = this.lookforward.shift();
            this.lastToken = n || Token.EOF
            return n || Token.EOF
        }
        let n = this.tokenizer.next()
        if (skipComments) {
            while (n.type == TokenType.comment) n = this.tokenizer.next()
        }
        this.lastToken = n
        return n
    }

    hasNext() {
        return this.tokenizer.canRead() || this.lookforward.length > 0
    }

    peek(count: number = 0, skipComments = true): Token {
        while (this.lookforward.length <= count && this.tokenizer.canRead()) {
            let n = this.tokenizer.next()
            if (skipComments) {
                while (n.type == TokenType.comment) n = this.tokenizer.next()
            }
            this.lookforward.push(n)
        }
        return this.lookforward[count] || Token.EOF
    }

    goBack() {
        if (this.lastToken.isValid()) {
            this.lookforward.unshift(this.lastToken)
        }
    }
    
    startRange(): RangeBuilder {
        return new RangeBuilder(this)
    }

    isNext(type: TokenType) {
        return this.peek().type == type
    }

    skip(type: TokenType) {
        if (this.isNext(type)) {
            this.next()
            return true
        }
        return false
    }

    /**
     * Checks if the next token's value matches any of the given values.
     * Returns true if it is
     * @param val The value or values to test
     */
    isValueNext(...val: string[]) {
        let peeked = this.peek().value
        for (let v of val) {
            if (v === peeked) return true
        }
        return false
    }
    
    /**
     * Skips the next token if it's value matches the given value
     * @param val The token value to skip
     * @returns True if we skipped, and false if we didn't
     */
    skipValue(val: string) {
        if (this.isValueNext(val)) {
            this.next()
            return true
        }
        return false
    }

    /**
     * Skips and returns the next token if it's of the given type.
     * If it doesn't, raise an error
     * @param type The token type we expect
     */
    expect(type: TokenType): Token | undefined {
        if (this.isNext(type)) {
            return this.next();
        }
        this.error(this.peek(),'Expected ' + TokenType[type])
        return undefined
    }

    /**
     * Skips and returns the next token if it's value matches the given value.
     * If it doesn't, raise an error
     * @param val The value we expect
     */
    expectValue(val: string): Token | undefined {
        if (this.isValueNext(val)) {
            return this.next();
        }
        this.error(this.peek(),'Expected ' + val)
        return undefined
    }

    /**
     * Skips and returns the next token if it matches any of the given values.
     * If it doesn't, raise an error
     * @param name A common label for what we're looking for
     * @param values The expected values
     */
    expectOneOf(name: string, ...values: string[]): Token {
        for (let v of values) {
            if (this.isValueNext(v)) return this.next()
        }
        this.error(this.peek(), "Expected " + name)
        return Token.invalid(this.peek().range)
    }

    /**
     * Skips any new line tokens until a different token is found or reached the end.
     */
    skipEmptyLines() {
        while (this.hasNext() && this.isNext(TokenType.line_end)) {
            this.next()
        }
    }

    /**
     * Parses an entire splash file and creates an AST (Abstract Syntax Tree)
     */
    parseFile(): RootNode {
        let root = new RootNode(this.file)

        while (this.hasNext()) {
            if (this.isNext(TokenType.line_end)) {
                this.next()
            } else {
                let s = this.parseTopLevel(new ModifierList())
                if (s) {
                    root.add(s)
                    if (this.hasNext()) {
                        this.expect(TokenType.line_end)
                    }
                } else {
                    while (this.hasNext() && !this.isNext(TokenType.line_end)) {
                        this.next()
                    }
                }
            }
        }

        return root
    }

    parseTopLevel(modifiers: ModifierList): ASTNode | undefined {
        if (this.isNext(TokenType.keyword)) {
            let kw = this.peek()
            switch (kw.value) {
                case 'var':
                    modifiers.assertEmpty(this)
                    return this.parseVarDecl()
                case 'main':
                    modifiers.assertEmpty(this)
                    this.next()
                    let block = this.parseBlock()
                    if (block) {
                        return new MainBlock(kw.range,block.statements)
                    }
                    break
                case 'private':
                case 'native':
                case 'abstract':
                    modifiers.add(this,kw)
                    this.next()
                    return this.parseTopLevel(modifiers)
                case 'function':
                    return this.parseFunction(modifiers)
                case 'class':
                    return this.parseClass(modifiers)
            }
        }
    }

    

    parseBlock(): CodeBlock | undefined {
        if (this.isValueNext('{')) {
            let block = new CodeBlock('block',this.next().range)
            while (this.hasNext()) {
                if (this.isNext(TokenType.line_end)) {
                    this.next()
                    continue
                } else if (this.isValueNext('}')) {
                    break
                }
                let s = this.parseStatement()
                if (s) {
                    block.statements.push(s)
                    if (this.hasNext()) {
                        this.expect(TokenType.line_end)
                    }
                } else {
                    while (this.hasNext() && !this.isNext(TokenType.line_end)) {
                        this.next()
                    }
                }
            }

            this.expectValue('}')
            return block
        }
        return undefined
    }

    parseClassBody(): ClassMember[] {
        let members: ClassMember[] = []
        if (this.skipValue('{')) {
            while (this.hasNext()) {
                if (this.isNext(TokenType.line_end)) {
                    this.next()
                    continue
                } else if (this.isValueNext('}')) {
                    break
                }
                let s = this.parseClassMember(new ModifierList())
                if (s) {
                    members.push(s)
                    if (this.hasNext()) {
                        this.expect(TokenType.line_end)
                    }
                } else {
                    while (this.hasNext() && !this.isNext(TokenType.line_end)) {
                        this.next()
                    }
                }
            }

            this.expectValue('}')
        }
        return members
    }

    parseClassMember(modifiers: ModifierList): ClassMember | undefined {
        let t = this.peek()
        if (t.type == TokenType.keyword) {
            let keys = Object.keys(Modifier)
            if (keys.includes(t.value)) {
                modifiers.add(this, t)
                this.next()
                return this.parseClassMember(modifiers)
            }
        }
        if (t.value == 'constructor') {
            this.next()
            return this.parseConstructor(modifiers)
        }
        let noName = modifiers.getOneOf(Modifier.indexer, Modifier.invoker, Modifier.iterator, Modifier.accessor, Modifier.assigner)
        let type: TypeToken | undefined
        if (this.peek(1).value == '(' && !noName) {
            type = BasicTypeToken.void
        } else {
            type = this.parseTypeToken(true)
            if (!type) return
        }
        let name;
        
        if (noName) {
            name = noName
        } else {
            name = this.expect(TokenType.identifier)
        }
        if (!name) return
        if (this.isValueNext('(')) {
            return this.parseMethod(modifiers, type, name)
        } else {
            return this.parseField(modifiers, type, name)
        }
    }

    parseMethod(modifiers: ModifierList, retType: TypeToken, name: Token): MethodNode {
        modifiers.assertHasOnly(this,Modifier.final,Modifier.private,Modifier.abstract,Modifier.accessor,Modifier.assigner,Modifier.get,Modifier.set,Modifier.indexer,Modifier.invoker,Modifier.iterator,Modifier.native,Modifier.operator,Modifier.protected,Modifier.static)
        modifiers.checkIncompatible(this,Modifier.final,Modifier.abstract)
        modifiers.checkIncompatible(this,Modifier.accessor,Modifier.invoker,Modifier.assigner,Modifier.iterator,Modifier.operator,Modifier.indexer)
        modifiers.checkIncompatible(this,Modifier.accessor,Modifier.invoker,Modifier.assigner,Modifier.iterator,Modifier.operator,Modifier.get,Modifier.set)
        modifiers.checkIncompatible(this,Modifier.private,Modifier.operator,Modifier.indexer,Modifier.iterator,Modifier.invoker,Modifier.accessor,Modifier.assigner)

        let params = this.parseList(this.parseParameter,'(',')')
        let body: CodeBlock | undefined
        if (this.isValueNext('{')) {
            body = this.parseBlock()
        }
        return new MethodNode(name,retType,params,modifiers,body)
    }

    parseField(modifiers: ModifierList, type: TypeToken, name: Token): FieldNode {
        modifiers.assertHasOnly(this,Modifier.final,Modifier.private,Modifier.readonly,Modifier.protected,Modifier.static)
        modifiers.checkIncompatible(this,Modifier.private,Modifier.protected)
        let defValue: Expression | undefined
        if (this.skipValue('=')) {
            defValue = this.parseExpression()
        }
        return new FieldNode(name, type, modifiers, defValue)
    }

    parseConstructor(modifiers: ModifierList) {
        modifiers.assertHasOnly(this,Modifier.private,Modifier.protected)
        modifiers.checkIncompatible(this,Modifier.private,Modifier.protected)
        let params = this.parseList(this.parseCtorParameter,'(',')')
        let body = this.parseBlock()
        return new ConstructorNode(params,modifiers,body)
    }

    parseStatement(): Statement | undefined {
        if (this.isValueNext('var')) {
            return this.parseVarDecl()
        } else if (this.isValueNext('if')) {
            return this.parseIf()
        } else if (this.isValueNext('{')) {
            return this.parseBlock()
        } else if (this.isValueNext('return')) {
            let label = this.next()
            let expr: Expression | undefined= this.parseExpression()
            if (expr instanceof InvalidExpression) {
                expr = undefined
            }
            return new ReturnStatement(label.range, expr)
        } else if (this.isValueNext('repeat')) {
            return this.parseRepeat()
        } else if (this.isValueNext('for')) {
            return this.parseFor()
        } else {
            return this.parseVarAccess()
        }
    }

    parseIf(): IfStatement | undefined {
        let label = this.next()
        if (this.expectValue('(')) {
            let expr = this.parseExpression()
            this.expectValue(')')
            this.skipEmptyLines()
            let then = this.parseStatement()
            if (then) {
                let orElse: ElseStatement | undefined
                this.skipEmptyLines()
                if (this.isValueNext('else')) {
                    let l = this.next()
                    this.skipEmptyLines()
                    let statement = this.parseStatement()
                    if (statement) {
                        orElse = new ElseStatement(l.range, statement)
                    } else {
                        this.error(this.peek(),"Expected else statement")
                    }
                } else {
                    this.goBack()
                }
                return new IfStatement(label.range, expr, then, orElse)
            } else {
                this.error(this.peek(),"Expected if statement")
            }
        }
    }

    parseRepeat(): Statement | undefined {
        let label = this.next()
        if (this.expectValue('(')) {
            let expr = this.parseExpression()
            this.expectValue(')')
            let run = this.parseStatement()
            if (run) {
                return new RepeatStatement(label, expr, run)
            }
        }
    }

    parseFor(): Statement | undefined {
        let label = this.next()
        if (this.expectValue('(') && this.expectValue('var')) {
            let varname = this.expect(TokenType.identifier)
            if (varname) {
                this.expectValue(':')
                let iter = this.parseExpression()
                this.expectValue(')')
                let then = this.parseStatement()
                if (then) {
                    return new ForStatement(label, varname, iter, then)
                }
            }
        }
    }

    parseFunction(modifiers: ModifierList): FunctionNode | undefined {
        modifiers.assertHasOnly(this,Modifier.private,Modifier.native)
        let label = this.next()
        let retType: TypeToken = BasicTypeToken.void
        
        if (this.peek(1).value != '(') {
            retType = this.parseTypeToken(true) || BasicTypeToken.void
        }
        let name = this.expect(TokenType.identifier)
        if (name && this.isValueNext('(')) {
            let params = this.parseList(this.parseParameter,'(',')')
            let code = this.parseBlock()
            return new FunctionNode(label.range, modifiers, name, retType, params, code)
        }
    }

    parseClass(modifiers: ModifierList): ClassDeclaration | undefined {
        this.next()
        modifiers.assertHasOnly(this,Modifier.private,Modifier.abstract,Modifier.final,Modifier.native)
        modifiers.checkIncompatible(this,Modifier.abstract,Modifier.final)
        let name = this.expect(TokenType.identifier)
        if (name) {
            let typeParams: TypeParameterNode[] = [];
            if (this.isValueNext('<')) {
                typeParams = this.parseList(this.parseTypeParam,'<','>')
            }
            let body = this.parseClassBody()
            return new ClassDeclaration(name,typeParams,body,modifiers)
        }
    }

    parseList<T>(parser: ()=>T | undefined, open: string, close: string): T[] {
        let values: T[] = []
        if (this.expectValue(open)) {
            while (this.hasNext() && !this.isValueNext(close)) {
                let p = parser.apply(this)
                if (p) {
                    values.push(p)
                }
                if (!this.skipValue(',')) {
                    break
                }
            }
            this.expectValue(close)
        }
        return values
    }

    parseTypeParam(): TypeParameterNode | undefined {
        let base = this.expect(TokenType.identifier)
            if (base) {
            let extend: TypeToken | undefined
            if (this.skipValue('extends')) {
                extend = this.parseTypeToken(false)
            }
            return new TypeParameterNode(base, extend)
        }
    }

    parseParameter(): ParameterNode | undefined {
        let type = this.parseTypeToken(true)
        if (type) {
            let name = this.expect(TokenType.identifier)
            if (name) {
                let vararg = this.skipValue('...')

                let expr: Expression | undefined
                if (!vararg && this.skipValue('=')) {
                    expr = this.parseExpression()
                }
                return new ParameterNode(name, type, expr, vararg)
            }
        }
    }

    parseCtorParameter(): ConstructorParamNode | undefined {
        let setToField = false
        let type: TypeToken | undefined
        if (this.skipValue('this')) {
            this.expectValue('.')
            setToField = true
        } else {
            type = this.parseTypeToken(true)
        }
        let name = this.expect(TokenType.identifier)
        if (name) {
            let vararg = this.skipValue('...')

            let expr: Expression | undefined
            if (!vararg && this.skipValue('=')) {
                expr = this.parseExpression()
            }
            return new ConstructorParamNode(name, setToField, type, expr, vararg)
        }
    }

    parseTypeToken(allowOptional: boolean = true): TypeToken | undefined {
        let first = this.parseSingleTypeToken(allowOptional)
        if (first) {
            if (this.isValueNext('|')) {
                let options = [first]
                while (this.hasNext() && this.skipValue('|')) {
                    let t = this.parseSingleTypeToken(allowOptional)
                    if (t) {
                        options.push(t)
                    } else {
                        break
                    }
                }
                return new ComboTypeToken(options)
            }
            return first
        }
    }

    parseSingleTypeToken(allowOptional: boolean): TypeToken | undefined {
        let range = this.startRange()
        let tok: TypeToken | undefined
        if (this.skipValue('(')) {
            tok = this.parseTypeToken(false)
            this.expectValue(')')
        }
        if (this.skipValue('function')) {
            let params = this.parseList(this.parseTypeToken,'(',')')
            let optional = allowOptional && this.skipValue('?')
            this.expectValue('=>')
            let ret = this.parseTypeToken(allowOptional)
            if (ret) {
                return new FunctionTypeToken(range.end(), params, ret, optional)
            }
        } else if (this.isNext(TokenType.identifier) || this.isNext(TokenType.keyword)) {
            let base = this.next()
            let params: TypeToken[] = []
            if (this.skipValue('<')) {
                while (this.hasNext() && !this.isValueNext('>')) {
                    let t = this.parseTypeToken(false)
                    if (t) {
                        params.push(t)
                    }
                    if (!this.skipValue(',')) {
                        break
                    }
                }
                this.expectValue('>')
            }
            tok = new BasicTypeToken(range.end(), base, params)
        }

        if (tok) {
            if (this.skipValue('[')) {
                this.expectValue(']')
                tok = new BasicTypeToken(range.end(),Token.dummy('array'),[tok])
            }
            
            tok.optional = allowOptional && this.skipValue('?')
            return tok
        }
    }

    parseVarDecl(): Statement | undefined {
        let tok = this.next()
        let name = this.expect(TokenType.identifier)
        if (name) {
            let expr: Expression | undefined
            if (this.skipValue('=')) {
                expr = this.parseExpression()
            }
            return new VarDeclaration(tok.range, name, expr)
        }
    }

    parseVarAccess(): Statement | undefined {
        let v = this.parsePrimaryExpression()
        if (v instanceof AssignableExpression) {
            let assignOp = this.expectOneOf('assignment operator',...Object.values(AssignmentOperator))
            let value = this.parseExpression()
            return new Assignment(v, assignOp, value)
        } else if (v instanceof CallAccess) {
            return new CallStatement(v, this.lastToken.range || TextRange.end)
        } else if (v instanceof InvalidExpression) {
            return
        }
        this.error(this.peek(),'Cannot assign to this expression')
    }

    parseAccessChain(parent: Expression): Expression {
        if (this.skipValue('.')) {
            let field = this.expect(TokenType.identifier)
            if (field) {
                return this.parseAccessChain(new FieldAccess(field, parent))
            }
        } else if (this.skipValue('(')) {
            let args = this.parseExpressionList(')');
            return this.parseAccessChain(new CallAccess(args, parent))
        } else if (this.skipValue('[')) {
            let index = this.parseExpression()
            this.expectValue(']')
            return this.parseAccessChain(new IndexAccess(index, parent))
        }
        return parent
    }

    parseExpressionList(end: string): ExpressionList {
        let range = this.startRange()
        let list: Expression[] = []
        while (this.hasNext() && !this.isValueNext(end)) {
            list.push(this.parseExpression())
            if (!this.skipValue(',')) {
                break
            }
        }
        this.expectValue(end)
        return new ExpressionList(list, range.end())
    }

    parseExpression(): Expression {
        let expr = this.parseOrExpression()
        while (this.isValueNext('&&')) {
            expr = new BinaryExpression(expr,this.next(),this.parseOrExpression());
        }
        return expr
    }

    parseOrExpression(): Expression {
        let expr = this.parseEqualityExpression()
        while (this.isValueNext('||')) {
            expr = new BinaryExpression(expr,this.next(),this.parseEqualityExpression());
        }
        return expr
    }

    parseEqualityExpression(): Expression {
        let expr = this.parseComparisonExpression()
        while (this.isValueNext('==','!=')) {
            expr = new BinaryExpression(expr,this.next(),this.parseComparisonExpression());
        }
        return expr
    }

    parseComparisonExpression(): Expression {
        let expr = this.parseAdditiveExpression()
        while (this.isValueNext('<','>','<=','>=','is')) {
            expr = new BinaryExpression(expr,this.next(),this.parseAdditiveExpression());
        }
        return expr
    }

    parseAdditiveExpression(): Expression {
        let expr = this.parseMultiExpression()
        while (this.isValueNext('+','-')) {
            expr = new BinaryExpression(expr,this.next(),this.parseMultiExpression());
        }
        return expr
    }

    parseMultiExpression(): Expression {
        let expr = this.parsePowExpression()
        while (this.isValueNext('*','/','//','%')) {
            expr = new BinaryExpression(expr,this.next(),this.parsePowExpression());
        }
        return expr
    }

    parsePowExpression(): Expression {
        let expr = this.parseIsInExpression()
        while (this.isValueNext('**')) {
            expr = new BinaryExpression(expr,this.next(),this.parseIsInExpression());
        }
        return expr
    }

    parseIsInExpression(): Expression {
        let expr = this.parseUnaryExpression()
        while (this.isValueNext('as','in','~')) {
            expr = new BinaryExpression(expr,this.next(),this.parseUnaryExpression());
        }
        return expr
    }

    

    parseUnaryExpression(): Expression {
        if (this.isValueNext('+','-','!','..')) {
            return new UnaryExpression(this.next(),this.parsePrimaryExpression());
        }
        return this.parsePrimaryExpression()
    }

    parsePrimaryExpression(): Expression {
        let expr: Expression
        if (this.isNext(TokenType.int) || this.isNext(TokenType.float) || this.isValueNext('true','false')) {
            expr = new LiteralExpression(this.next())
        } else if (this.isNext(TokenType.string)) {
            let tok = this.next() as StringToken
            expr = new StringExpression(tok.range,tok.segments.map(s=>{
                if (s instanceof ExpressionSegment) {
                    return new Parser(this.file,new DelegateTokenizer(s.tokens)).parseExpression()
                } else {
                    return new LiteralExpression(new Token(TokenType.string,s.toString(),TextRange.end))
                }
            }))
        } else if (this.isNext(TokenType.identifier)) {
            expr = new VariableAccess(this.next())
        } else if (this.skipValue('[')) {
            let values = this.parseExpressionList(']')
            expr = new ArrayExpression(values)
        } else if (this.skipValue('(')) {
            expr = this.parseExpression()
            this.expectValue(')')
        } else if (this.isValueNext('this')) {
            expr = new ThisAccess(this.next())
        } else if (this.isValueNext('null')) {
            expr = new NullExpression(this.next())
        } else {
            return new InvalidExpression()
        }
        return this.parseAccessChain(expr)
        /* todo: add other types of expression
        json object
        */
    }

}

export class RangeBuilder {

    start: Position

    constructor(private parser: Parser) {
        this.start = parser.peek().range.start
    }

    end(): TextRange {
        return {start: this.start, end: this.parser.lastToken.range.end}
    }
}