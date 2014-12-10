"Author: Brandin Jefferson
CLID: bej0843
Date: December 08, 2014
Program Description: A smalltalk program that emulates a C++ Switch statement."

"Description: Represents a case or a series of cases."
Object subclass: Case [
"	Name: break
	Type: Boolean
	Description: Represents the break statement. If set to true, then it represents there being a break statement.

	Name: constants
	Type: OrderedCollection
	Description: Represent the case or cases attached to the block. 

	Name: block
	Type: BlockClosure
	Description: The set of statements to be associated with the cases."
	|break constants block|
	<comment: 'I represent a C++ case (or set of cases) to be used in a C++ switch statement.'>
	
	"Description: Creates a new instance of Case, initializes its instance variables, and returns the object."
	new [
		|s|
		<category: 'instance creation'>
		s := super new . 
		s initialize . 
		^s
	]
	"Description: Initializes the instance variables."
	init [
		break := false .
		constants := OrderedCollection new .
		block := nil
	]
	"Description: Overrides Object's printOn message. This was overridden to make it easier to see the default of the switch."
	printOn:stream[
		super printOn: stream .
		stream nextPutAll: ' with constants: ' .
		constants printOn: stream .
		stream nextPutAll: ' block: ' .
		block printOn: stream .
		stream nextPutAll: ' and break: ' .
		break printOn: stream 
	]
	"Description: Basically initializes a case object. The case argument is the constant, while the aBlock argument and aBool arguments represent the code and break statement respectively."
	add:case block:aBlock break:aBool [
		constants add:case .
		block := aBlock .
		break := aBool
	]
	"Description: Add a list of constants to create a case object containing them before adding the case to the end of the aSwitch object."
	addAll:cases block:aBlock break:aBool [
		cases do: [:each | constants add:each] .
		block := aBlock .
		break := aBool
	]
	"Description: Returns whether the case object has a constant that is equal to the given expression."
	hasConstant:expression [
		^constants includes:expression
	]
	"Description: Returns whether this case has a break associated with it."
	hasBreak [
		^break
	]
	"Description: Returns the block associated with the case."
	block [
		^(block value)
	]
	"Description: Returns the list of constants. For debugging purposes only."
	constants [
		^constants
	]
]

"Description: Represents a C++ Switch statement."
Object subclass: CSwitch [
"	Name: aSwitch
	Type: Dictionary
	Description: Represents the body of the switch statement, excluding the default statement. The keys of the dictionary are considered cases and their values are the statements they correspond to.

	Name: default
	Type: blockClosure 
	Description: This is the default statement that is called whenever a case is called for that does not exist. It can be made nil, but that is not advised."
	|default aSwitch|
	<comment: 'I represent a C++ switch statement.'>
	
	"Description: Creates a new instance of CSwitch, initializes its instance variables, and returns the object."
	new [
		|s|
		<category: 'instance creation'>
		s := super new . 
		s init. 
		^s
	]
	"Description: Initializes the instance variables."
	init [
		<category: 'initialization'>
		aSwitch := OrderedCollection new .
		default := nil
	]
	"Description: Overrides Object's printOn message. This was overridden to make it easier to see the default of the switch."
	printOn: stream [
		<category: 'printing'>
		super printOn: stream .
		stream nextPutAll: ' with a default: ' .
		default printOn: stream
	]
	"Description: Sets the default variable."
	setDefault:aBlock [
		default := aBlock
	]
	"Description: Adds a case to aSwitch by creating a case object, initializing it with the given arguments, and then adding to the list."
	add:case block:aBlock break:aBool [
		|aCase|
		aCase := Case new. aCase init.
		aCase add:case block:aBlock break:aBool .
		aSwitch addLast:aCase
	]
	"Description: Adds a list of cases to the aSwitch object as a list. Attached to that list is a single block and optional break."
	addAll:cases block:aBlock break:aBool [
		|aCase|
		aCase := Case new. aCase init.
		aCase addAll:cases block:aBlock break:aBool .
		aSwitch addLast:aCase
	]
	"Description: Evaluates the expression and iterates through the aSwitch object until the case equivalent to the expression is found. Otherwise, the default statement is returned. If the default statement was not initialized, then nothing is given as output. "
	switch:expression [
		|reached|
		reached := false .
		
		aSwitch do: [:case |
				((case hasConstant:expression) | reached) 
					ifTrue:[ (case hasBreak) 
						ifTrue:[^(case block)]
						ifFalse:[reached := true . (case block)]]] .
		default ifNotNil: [^default]
	]
	"Description: Returns the aSwitch object. This is really only for debugging purposes or if you need to see the cases."
	aSwitch[
		^aSwitch
	]
	"Description: Returns the default object. This is really only for debugging purposes or if you need to see the default case."
	default [
		^default
	]
]

