package org.uqbar.thin.less.modules

trait LambdaCalculus extends Expressions {
	case class Lambda(argument: Variable, body: Expression) extends Expression
	case class Application(left: Expression, right: Expression) extends Expression
	case class Variable(name: String) extends Expression
}

//▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
// ADAPTERS
//▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄

trait LambdaCalculusSourceability extends ExpressionSourceability { on: LambdaCalculus =>
	protected def applicable = variable | lambda | parens
	protected def variable = "[a-zA-Z_]+".r ^^ ({ (_: Variable).name }, Variable)
	protected def lambda = ('lambdaHeader ~> variable <~ 'lambdaSeparator) ~ expression ^^ Lambda
	protected def parens = 'openParens ~> expression <~ 'closeParens
	protected def applicationChain = ('application ~> applicable).*
	override protected def expression: Grammar[Expression] = applicable ~ applicationChain ^^ ((
		(e: Expression) => {
			val a :: ac = {
				def flattenApp(e: Expression): List[Expression] = e match {
					case Application(r, a) => flattenApp(r) ::: List(a)
					case _ => List(e)
				}
				flattenApp(e)
			}
			(a, ac)
		},
		{ p: (Expression, List[Expression]) => (p._1 /: p._2)(Application) }
	))
}