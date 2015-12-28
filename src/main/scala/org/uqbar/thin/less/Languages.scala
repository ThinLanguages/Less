package org.uqbar.thin.less

import org.uqbar.thin.more.Language
import org.uqbar.thin.less.modules._

object LambdaLess extends Language
	with Expressions with ExpressionSourceability
	with LambdaCalculus with LambdaCalculusSourceability