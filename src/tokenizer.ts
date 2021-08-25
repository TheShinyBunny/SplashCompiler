
export interface Position {
    line: number
    column: number
}

export interface TextRange {
    start: Position
    end: Position
}

export namespace TextRange {
    
    export function atLine(line: number, start: number, end: number): TextRange {
        return {start: {line,column: start}, end: {line, column: end}}
    }

    export function toString(range: TextRange) {
        if (isInOneLine(range)) {
            return range.start.line + ", " + range.start.column + "-" + range.end.column;
        }
        return range.start.line + ":" + range.start.column + "-" + range.end.line + ":" + range.end.column;
    }

    export function isInOneLine(range: TextRange) {
        return range.start.line == range.end.line
    }

    export function contains(range: TextRange, pos: Position) {
        return range.start.line <= pos.line && range.start.column <= pos.column
            && range.end.line >= pos.line && range.end.column >= pos.column;
    }

    export function oneChar(pos: Position): TextRange {
        return plus(pos,1)
    }

    export function plus(pos: Position, count: number): TextRange {
        return {start: pos, end: {line: pos.line, column: pos.column + count}}
    }

    export function min(a: TextRange, b: TextRange) {
        if (a.start.line < b.start.line) {
            return a
        } else if (a.start.line > b.start.line) {
            return b
        }
        if (a.start.column < b.start.column) {
            return a
        } else if (a.start.column > b.start.column) {
            return b
        }
        return a
    }

    export function max(a: TextRange, b: TextRange) {
        if (a.start.line < b.start.line) {
            return b
        } else if (a.start.line > b.start.line) {
            return a
        }
        if (a.start.column < b.start.column) {
            return b
        } else if (a.start.column > b.start.column) {
            return a
        }
        return b
    }

    export function between(from: TextRange, to: TextRange): TextRange {
        let first = min(from, to)
        let second = max(from, to)
        return {start: first.start, end: second.end}
    }

    export const end: TextRange = {start: {line: -1, column: 0}, end: {line: -1, column: 0}}

}

export enum TokenType {
    keyword,
    identifier,
    symbol,
    int,
    float,
    string,
    line_end,
    comment,
    invalid
}

export class Token {

    static EOF = Token.invalid(TextRange.end)

    constructor(public type: TokenType, public value: string, public range: TextRange) {

    }

    isValid() {
        return this.type != TokenType.invalid
    }

    toString() {
        return TokenType[this.type] + ':' + this.value + '(' + TextRange.toString(this.range) + ')'
    }

    static invalid(range: TextRange) {
        return new Token(TokenType.invalid, "", range)
    }

    static dummy(value: string) {
        let t = new BaseTokenizer(value)
        return t.next()
    }

    static empty(range: TextRange) {
        return new Token(TokenType.invalid, '', range)
    }
}

export class StringToken extends Token {
    constructor(value: string, public segments: StringSegment[], range: TextRange) {
        super(TokenType.string, value, range)
    }

    toString() {
        return 'string:' + this.segments.join('') + '(' + TextRange.toString(this.range) + ')'
    }

}

export interface StringSegment {

}

export class LiteralSegment implements StringSegment {
    value: string = ""

    toString() {
        return this.value
    }
}

export class ExpressionSegment implements StringSegment {
    tokens: Token[] = []
    
    toString() {
        return '{' + this.tokens + '}'
    }
}

const symbols = ['~','`',';','!','@','#','$','%','^','&','*','(',')','-','+','=','[',']','{','}','\'',':','"',',','?','/','|','\\','.','<','>'];

const multiCharOperators = ['++','--','&&','||','**','//','<=','>=','==','!=','..','...','+=','-=','*=','/=','%=','**=','//='];

const keywords = ['main','function','if','this','var','const','as','is','in','while','for','switch','repeat','class','return','constructor','private','protected','abstract','native','final','static','readonly','operator','iterator','get','set','indexer','accessor','assigner','invoker','true','false','null','void']

const escapableChars: {[key: string]: string} = {
    '\\': '\\',
    '"': '"',
    "'": "'",
    '{': '{',
    'n': '\n',
    'b': '\b',
    'r': '\r',
    't': '\t',
    'f': '\f'
}

export interface Tokenizer {

    next(): Token

    canRead(): boolean

}

export class BaseTokenizer implements Tokenizer {

    pos: number = 0;
    line: number = 1;
    column: number = 1;
    currentStringSegment?: ExpressionSegment

    constructor(private input: string) {
        
    }

    next(): Token {
        if (!this.canRead()) Token.EOF
        let start: Position = {line: this.line,column: this.column}
        let c = this.nextChar()
        
        switch (c) {
            case ' ':
                while (c == ' ') {
                    c = this.nextChar()
                }
                this.pos--
                this.column--
                return this.next()
            case '\r':
                return this.next()
            case '\n':
                let t = new Token(TokenType.line_end, '\n', TextRange.oneChar(start))
                this.line++;
                this.column = 1;
                return t;
            case '"':
            case '\'':
                return this.readString(start,c)
            case '#':
                let comment = '';
                while (this.canRead()) {
                    c = this.nextChar()
                    if (c == '\n') {
                        this.pos--
                        this.column--
                        break
                    }
                    comment += c
                }
                return new Token(TokenType.comment, comment, {start, end: this.getPos()})
            case '/':
                let blockComment = '';
                let next = this.nextChar();
                if (next == '*') {
                    while (this.canRead()) {
                        next = this.nextChar()
                        if (next == '*') {
                            next = this.nextChar()
                            if (next == '/') {
                                break
                            }
                            blockComment += '*' + next
                        } else {
                            blockComment += next
                            if (next == '\n') {
                                this.line++
                                this.column = 1
                            }
                        }
                    }
                    return new Token(TokenType.comment, blockComment, {start, end: this.getPos()})
                }
                this.column--
                this.pos--
            default:
                if (/[0-9]/.test(c)) {
                    return this.readNumber(start, c)
                }
                if (/[a-zA-Z_]/.test(c)) {
                    return this.readIdentifier(start, c)
                }
                if (symbols.includes(c)) {
                    return this.readSymbol(start, c)
                }
                return Token.invalid({start, end: this.getPos()})
        }
    }

    canRead() {
        return this.pos < this.input.length
    }

    nextChar() {
        this.column++;
        return this.input[this.pos++]
    }

    getPos(): Position {
        return {line: this.line, column: this.column}
    }

    readString(start: Position, quote: string): Token {
        let segments: StringSegment[] = []
        let current: StringSegment | undefined

        while (this.canRead()) {
            let c = this.nextChar()
            if (c == '{') {
                current = new ExpressionSegment()
                segments.push(current)
                let tok = this.next()
                let stack = 0;
                while (this.canRead() && tok.isValid() && (stack > 0 || tok.value != '}')) {
                    (current as ExpressionSegment).tokens.push(tok)
                    if (tok.value == '{') stack++
                    else if (tok.value == '}') stack--
                    tok = this.next()
                }
            } else if (c != quote) {
                if (!(current instanceof LiteralSegment)) {
                    current = new LiteralSegment()
                    segments.push(current)
                }
                if (c == '\\' && this.canRead()) {
                    let next = this.nextChar()
                    if (escapableChars[next]) {
                        (current as LiteralSegment).value += escapableChars[next];
                    } else {
                        (current as LiteralSegment).value += c + next
                    }
                } else {
                    (current as LiteralSegment).value += c;
                }
            } else {
                break
            }
        }
        return new StringToken('string(' + segments.join('') + ')',segments, {start, end: this.getPos()})
    }

    readNumber(start: Position, first: string) {
        let str = first;
        while (this.canRead()) {
            let c = this.nextChar()
            if (/[0-9]/.test(c)) {
                str += c;
            } else if (c == '.') {
                if (!str.includes('.')) {
                    str += '.'
                } else if (str[str.length - 1] == '.') {
                    str = str.substring(0,str.length - 1)
                    this.column -= 2;
                    this.pos -= 2;
                    break
                } else {
                    this.column--;
                    this.pos--;
                    break
                }
            } else {
                this.column--;
                this.pos--;
                break
            }
        }
        if (str.indexOf('.') < 0) {
            return new Token(TokenType.int, str, {start, end: this.getPos()})
        } else {
            return new Token(TokenType.float, str, {start, end: this.getPos()})
        }
    }

    readIdentifier(start: Position, first: string) {
        let val = first
        while (this.canRead()) {
            let c = this.nextChar()
            if (/[a-zA-Z_0-9]/.test(c)) {
                val += c
            } else {
                this.column--
                this.pos--
                break
            }
        }
        if (keywords.includes(val)) {
            return new Token(TokenType.keyword, val, {start, end: this.getPos()})
        }
        return new Token(TokenType.identifier, val, {start, end: this.getPos()})
    }

    readSymbol(start: Position, first: string) {
        let op = first
        let lastValid = op
        while (this.canRead()) {
            let c = this.nextChar()
            if (symbols.includes(c)) {
                op += c
                if (multiCharOperators.includes(op)) {
                    lastValid = op
                }
            } else {
                this.column--
                this.pos--
                break
            }
        }
        this.column -= (op.length - lastValid.length)
        this.pos -= (op.length - lastValid.length)
        return new Token(TokenType.symbol, lastValid, {start, end: this.getPos()})
    }
}

export class DelegateTokenizer implements Tokenizer {

    pos: number = 0
    constructor(public tokens: Token[]) {
        
    }

    next(): Token {
        if (this.pos >= this.tokens.length) {
            return Token.EOF
        }
        return this.tokens[this.pos++]
    }
    canRead(): boolean {
        return this.pos < this.tokens.length
    }

}