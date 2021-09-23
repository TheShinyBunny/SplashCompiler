import { Diagnostic, DiagnosticType } from ".";
import { ElseStatement, NullExpression, ArrayExpression, AssignableExpression, Assignment, BinaryExpression, CallAccess, CallStatement, CodeBlock, Expression, FieldAccess, IfStatement, InvalidExpression, LiteralExpression, MainBlock, RootNode, Statement, UnaryExpression, VarDeclaration, VariableAccess, ModifierList, ParameterNode, FunctionNode, ReturnStatement, ExpressionList, StringExpression, ClassDeclaration, ClassMember, MethodNode, FieldNode, ConstructorParamNode, ConstructorNode, ThisAccess, ASTNode, IndexAccess, TypeParameterNode, RepeatStatement, ForStatement, WhileStatement, BreakContinueStatement, StatementExpression, ExpressionStatement, SwitchCase, SwitchStatement, CaseConstraint, VariableCaseConstraint, ArrayCaseConstraint, LiteralCaseConstraint, InvalidCaseConstraint, OrCaseConstraint } from "./ast";
import { Completion, CompletionType, PartialCompletion, TextLocation } from "./env";
import { BasicTypeToken, ComboTypeToken, FunctionTypeToken, SingleTypeToken, TypeToken } from "./oop";
import { AssignmentOperator, BinaryOperator, Modifier } from "./operators";
import { DelegateTokenizer, ExpressionSegment, LiteralSegment, Position, StringToken, TextRange, Token, Tokenizer, TokenType } from "./tokenizer";

export class Parser {

    lookforward: Token[] = []
    lastToken: Token
    diagnostics: Diagnostic[] = []
    cursor?: Position
    completionItems: Completion[] = []
    canUseBreakContinue = false

    constructor(public file: string, public tokenizer: Tokenizer) {
        this.lastToken = Token.EOF;
    }

    error(token: Token, msg: string) {
        this.errorRange(token.range, msg)
    }

    errorRange(range: TextRange, msg: string) {
        console.trace('Compilation error at ' + TextRange.toString(range) + ': ' + msg)
        this.diagnostics.push({file: this.file, range, message: msg, type: DiagnosticType.error})
    }

    suggest(range: TextRange, items: ()=>PartialCompletion[]) {
        if (this.cursor && TextRange.contains(range, this.cursor)) {
            this.completionItems.push(...items().map(c=>({range, ...c})))
        }
    }

    suggestHere(type: CompletionType, ...items: string[]) {
        this.suggest(this.peek().range, ()=>items.map(s=>({value: s, type})))
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

    location(range: TextRange): TextLocation {
        return {range, file: this.file}
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
    expectValue(val: string, suggest?: CompletionType): Token | undefined {
        if (suggest) {
            this.suggestHere(suggest,val)
        }
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

        let comment: string | undefined = undefined
        while (this.hasNext()) {
            if (this.peek(0,false).type == TokenType.line_end) {
                this.next(false)
            } else if (this.peek(0,false).type == TokenType.comment) {
                comment = this.next(false).value
            } else {
                let s = this.parseTopLevel(new ModifierList(),comment)
                if (s) {
                    root.add(s)
                    if (this.hasNext()) {
                        this.expect(TokenType.line_end)
                    }
                } else {
                    let range = this.startRange()
                    while (this.hasNext() && !this.isNext(TokenType.line_end)) {
                        this.next()
                    }
                    this.errorRange(range.end(),'Invalid statement')
                }
                comment = undefined
            }
        }

        if (this.diagnostics.length > 0) {
            root.valid = false
        }

        return root
    }

    parseTopLevel(modifiers: ModifierList, comment?: string): ASTNode | undefined {
        this.suggestHere(CompletionType.keyword,'var','main','function','class','private','abstract')
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
                    this.next(false)
                    return this.parseTopLevel(modifiers,comment)
                case 'function':
                    let func = this.parseFunction(modifiers)
                    if (func) {
                        func.docs = comment
                    }
                    return func
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
                    let range = this.startRange()
                    while (this.hasNext() && !this.isNext(TokenType.line_end)) {
                        this.next()
                    }
                    this.errorRange(range.end(),'Invalid statement')
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
            let comment: string | undefined = undefined
            while (this.hasNext()) {
                if (this.peek(0,false).type == TokenType.line_end) {
                    this.next(false)
                    continue
                } else if (this.peek(0,false).value == '}') {
                    break
                } else if (this.peek(0,false).type == TokenType.comment) {
                    comment = this.next(false).value
                    continue
                }
                let s = this.parseClassMember(new ModifierList(),comment)
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
                comment = undefined
            }

            this.expectValue('}')
        }
        return members
    }

    parseClassMember(modifiers: ModifierList, comment?: string): ClassMember | undefined {
        let t = this.peek()
        if (t.type == TokenType.keyword) {
            let keys = Object.keys(Modifier)
            if (keys.includes(t.value)) {
                modifiers.add(this, t)
                this.next()
                return this.parseClassMember(modifiers,comment)
            }
        }
        if (t.value == 'constructor') {
            let ctor = this.parseConstructor(modifiers)
            ctor.docs = comment
            return ctor
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
            let m = this.parseMethod(modifiers, type, name)
            m.docs = comment
            return m
        } else {
            let f = this.parseField(modifiers, type, name)
            f.docs = comment
            return f
        }
    }

    parseMethod(modifiers: ModifierList, retType: TypeToken, name: Token): MethodNode {
        modifiers.assertHasOnly(this,Modifier.final,Modifier.private,Modifier.abstract,Modifier.accessor,Modifier.assigner,Modifier.get,Modifier.set,Modifier.indexer,Modifier.invoker,Modifier.iterator,Modifier.native,Modifier.operator,Modifier.bidir,Modifier.protected,Modifier.static)
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
        let label = this.next()
        modifiers.assertHasOnly(this,Modifier.private,Modifier.protected)
        modifiers.checkIncompatible(this,Modifier.private,Modifier.protected)
        let params = this.parseList(this.parseCtorParameter,'(',')')
        let body = this.parseBlock()
        return new ConstructorNode(label.range,params,modifiers,body)
    }

    parseStatement(): Statement | undefined {
        this.suggestHere(CompletionType.keyword,'var','if','return','repeat','for','while')
        if (this.isValueNext('var')) {
            return this.parseVarDecl()
        } else if (this.isValueNext('if')) {
            let stm = this.parseIf()
            if (stm) return new ExpressionStatement(stm)
        } else if (this.isValueNext('{')) {
            return this.parseBlock()
        } else if (this.isValueNext('return')) {
            let label = this.next()
            let expr: Expression | undefined= this.parseExpression()
            if (expr instanceof InvalidExpression) {
                expr = undefined
            }
            return new ReturnStatement(label.range, expr)
        } else if (this.isValueNext('break')) {
            let label = this.next()
            if (!this.canUseBreakContinue) {
                this.error(label, "Break cannot be used outside a for, while or repeat statement.")
            }
            return new BreakContinueStatement(label)
        } else if (this.isValueNext('continue')) {
            let label = this.next()
            if (!this.canUseBreakContinue) {
                this.error(label, "Continue cannot be used outside a for, while or repeat statement.")
            }
            return new BreakContinueStatement(label)
        } else if (this.isValueNext('repeat')) {
            return this.parseRepeat()
        } else if (this.isValueNext('for')) {
            return this.parseFor()
        } else if (this.isValueNext('while')) {
            return this.parseWhile()
        } else if (this.isValueNext('switch')) {
            let sw = this.parseSwitch()
            if (sw) return new ExpressionStatement(sw)
        } else {
            return this.parseVarAccess()
        }
    }

    parseStatementAsExpression() {
        let expr = this.parseExpression()
        if (expr instanceof InvalidExpression) {
            let stm = this.parseStatement()
            if (stm instanceof ExpressionStatement) {
                return stm.expr
            } else if (stm) {
                return new StatementExpression(stm)
            }
        }
        return expr
    }

    parseIf(): IfStatement | undefined {
        let label = this.next()
        if (this.expectValue('(')) {
            let expr = this.parseExpression()
            this.expectValue(')')
            this.skipEmptyLines()
            let then = this.parseStatementAsExpression()
            let orElse: Expression | undefined
            this.skipEmptyLines()
            this.suggestHere(CompletionType.keyword,'else')
            if (this.isValueNext('else')) {
                this.next()
                this.skipEmptyLines()
                orElse = this.parseStatementAsExpression()
                if (!orElse) {
                    this.error(this.peek(),"Expected else statement")
                }
            } else {
                this.goBack()
            }
            return new IfStatement(label.range, expr, then, orElse)
        }
    }

    parseRepeat(): Statement | undefined {
        let label = this.next()
        if (this.expectValue('(')) {
            let expr = this.parseExpression()
            this.expectValue(')')
            let before = this.canUseBreakContinue
            
            this.canUseBreakContinue = true
            let run = this.parseStatement()
            this.canUseBreakContinue = before

            if (run) {
                return new RepeatStatement(label, expr, run)
            }
        }
    }

    parseFor(): Statement | undefined {
        let label = this.next()
        if (this.expectValue('(') && this.expectValue('var',CompletionType.keyword)) {
            let varname = this.expect(TokenType.identifier)
            if (varname) {
                this.expectValue(':')
                let iter = this.parseExpression()
                this.expectValue(')')

                let before = this.canUseBreakContinue
            
                this.canUseBreakContinue = true
                let then = this.parseStatement()
                this.canUseBreakContinue = before

                if (then) {
                    return new ForStatement(label, varname, iter, then)
                }
            }
        }
    }

    parseWhile(): Statement | undefined {
        let label = this.next()
        if (this.expectValue('(')) {
            let expr = this.parseExpression()
            this.expectValue(')')

            let before = this.canUseBreakContinue
            
            this.canUseBreakContinue = true
            let run = this.parseStatement()
            this.canUseBreakContinue = before
            if (run) {
                return new WhileStatement(label, expr, run)
            }
        }
    }

    parseSwitch(): SwitchStatement | undefined {
        let label = this.next()
        if (this.expectValue('(')) {
            let expr = this.parseExpression()
            this.expectValue(')')

            this.expectValue('{')

            let cases: SwitchCase[] = []
            let defCase: Expression | undefined
            while (this.hasNext()) {
                if (this.isNext(TokenType.line_end)) {
                    this.next()
                    continue
                } else if (this.isValueNext('}')) {
                    break
                }

                this.suggestHere(CompletionType.keyword, 'case', 'default')
                
                if (this.isValueNext('case')) {
                    let c = this.parseSwitchCase()
                    cases.push(c)
                    if (this.hasNext()) {
                        this.expect(TokenType.line_end)
                    }
                } else if (this.isValueNext('default')) {
                    let lbl = this.next()
                    let exp = this.parseStatementAsExpression()
                    if (defCase) {
                        this.error(lbl, 'Cannot have multiple default cases in a switch')
                    } else {
                        defCase = exp
                    }
                } else {
                    let range = this.startRange()
                    while (this.hasNext() && !this.isNext(TokenType.line_end)) {
                        this.next()
                    }
                    this.errorRange(range.end(),'Invalid statement')
                }
            }

            this.expectValue('}')

            return new SwitchStatement(label.range, expr, cases, defCase)
        }
    }

    parseFunction(modifiers: ModifierList): FunctionNode | undefined {
        modifiers.assertHasOnly(this,Modifier.private,Modifier.native)
        this.next()
        let retType: TypeToken = BasicTypeToken.void
        
        if (this.peek(1).value != '(') {
            retType = this.parseTypeToken(true) || BasicTypeToken.void
        }
        let name = this.expect(TokenType.identifier)
        if (name && this.isValueNext('(')) {
            let params = this.parseList(this.parseParameter,'(',')')
            let code = this.parseBlock()
            return new FunctionNode(this.location(name.range), modifiers, name, retType, params, code)
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
            this.suggestHere(CompletionType.keyword, 'extends')
            let extend: TypeToken | undefined
            if (this.skipValue('extends')) {
                extend = this.parseTypeToken(false)
            }
            let body = this.parseClassBody()
            return new ClassDeclaration(name,typeParams,body,modifiers,extend)
        }
    }

    parseList<T>(parser: ()=>T | undefined, open: string, close: string): T[] {
        let values: T[] = []
        if (open == '' || this.expectValue(open)) {
            while (this.hasNext() && (close == '' || !this.isValueNext(close))) {
                let p = parser.apply(this)
                if (p) {
                    values.push(p)
                }
                if (!this.skipValue(',')) {
                    break
                }
            }
            if (close != '') {
                this.expectValue(close)
            }
        }
        return values
    }

    parseTypeParam(): TypeParameterNode | undefined {
        let base = this.expect(TokenType.identifier)
        if (base) {
            let extend: TypeToken | undefined
            this.suggestHere(CompletionType.keyword,'extends')
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
        this.suggestHere(CompletionType.keyword,'function')
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
            if (this.skipValue('++')) {
                return new Assignment(v,Token.dummy('+='),new LiteralExpression(Token.dummy("1")))
            }
            if (this.skipValue('--')) {
                return new Assignment(v,Token.dummy('-='),new LiteralExpression(Token.dummy("1")))
            }
            let assignOp = this.expectOneOf('assignment operator',...Object.values(AssignmentOperator))
            let value = this.parseExpression()
            return new Assignment(v, assignOp, value)
        } else if (v instanceof CallAccess) {
            return new CallStatement(v, this.lastToken.range || TextRange.end)
        } else if (v instanceof InvalidExpression) {
            return
        }
    }

    parseAccessChain(parent: Expression): Expression {
        if (this.skipValue('.')) {
            let field = this.expect(TokenType.identifier)
            if (field) {
                return this.parseAccessChain(new FieldAccess(field, parent))
            }
            return new FieldAccess(Token.empty(this.peek().range),parent)
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
        let expr = this.parseAsInExpression()
        while (this.isValueNext('**')) {
            expr = new BinaryExpression(expr,this.next(),this.parseAsInExpression());
        }
        return expr
    }

    parseAsInExpression(): Expression {
        let expr = this.parseRangeExpression()
        while (this.isValueNext('as','in') || (this.isValueNext('!') && (this.peek(1).value == 'as' || this.peek(1).value == 'in'))) {
            let negToken = this.next()
            expr = new BinaryExpression(expr,negToken.value == '!' ? this.next() : negToken,this.parseRangeExpression());
            if (negToken.value == '!') {
                expr = new UnaryExpression(negToken,expr)
            }
        }
        return expr
    }

    parseRangeExpression(): Expression {
        let expr = this.parseUnaryExpression()
        while (this.isValueNext('..','~')) {
            expr = new BinaryExpression(expr,this.next(),this.parseUnaryExpression());
        }
        return expr
    }

    parseUnaryExpression(): Expression {
        if (this.isValueNext('+','-','!')) {
            return new UnaryExpression(this.next(),this.parsePrimaryExpression());
        }
        return this.parsePrimaryExpression()
    }

    parsePrimaryExpression(): Expression {
        let expr: Expression
        this.suggestHere(CompletionType.keyword,'this','null','true','false')
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
        } else if (this.isValueNext('if')) {
            return this.parseIf() || InvalidExpression.instance
        } else if (this.isValueNext('switch')) {
            return this.parseSwitch() || InvalidExpression.instance
        } else {
            return InvalidExpression.instance
        }
        return this.parseAccessChain(expr)
        /* todo: add other types of expression
        json object
        */
    }

    parseSwitchCase(): SwitchCase {
        let label = this.next()
        let constraints = this.parseList(this.parseCaseExpr,'','')
        let then = this.parseStatementAsExpression()
        return new SwitchCase(label.range,constraints,then)
    }

    parseCaseExpr(): CaseConstraint {
        let con = this.parseSingleCaseExpr()
        let orList = [con]
        while (this.isValueNext('||')) {
            this.next()
            let other = this.parseSingleCaseExpr()
            orList.push(other)
        }

        return orList.length == 1 ? con : new OrCaseConstraint(orList)
    }

    parseSingleCaseExpr(): CaseConstraint {
        // todo: consider adding the labeled case, which is kinda useless
        if (this.skipValue('*')) {
            let name = this.expect(TokenType.identifier);
            if (name) { 
                return new VariableCaseConstraint(name,true)
            }
            return InvalidCaseConstraint.instance
        }
        if (this.isNext(TokenType.identifier)) {
            return new VariableCaseConstraint(this.next(),false)
        }
        if (this.isValueNext('[')) {
            let range = this.startRange()
            let list = this.parseList(this.parseCaseExpr,'[',']')
            return new ArrayCaseConstraint(range.end(),list)
        }
        if (this.isNext(TokenType.int) || this.isNext(TokenType.float) || this.isValueNext('true','false')) {
            let expr = new LiteralExpression(this.next())
            return new LiteralCaseConstraint(expr)
        } else if (this.isNext(TokenType.string)) {
            let tok = this.next() as StringToken
            let expr = new StringExpression(tok.range,tok.segments.map(s=>{
                if (s instanceof ExpressionSegment) {
                    return new Parser(this.file,new DelegateTokenizer(s.tokens)).parseExpression()
                } else {
                    return new LiteralExpression(new Token(TokenType.string,s.toString(),TextRange.end))
                }
            }))
            return new LiteralCaseConstraint(expr)
        }
        this.error(this.peek(),'Invalid case constraint')
        return InvalidCaseConstraint.instance
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