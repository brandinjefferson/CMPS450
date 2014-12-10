"Author: Brandin Jefferson
CLID: bej0843
Date: December 08, 2014
Program Description: A smalltalk program that emulates a C++ Switch statement."

Object subclass: CSwitch [
	"Description: instance variables.
	default - The default block statement.
	aSwitch - A dictionary; used to emulate the body of a switch by attaching key values to a block"
	|default aSwitch|
	<comment: 'I represent a C++ switch statement.'>
	
	"Description: Creates a new instance of CSwitch, initializes its instance variables and returns
		the object."
	new [
		|s|
		<category: 'instance creation'>
		s := super new . 
		s init . 
		^s
	]
	"Description: Initializes the instance variables. "
	init [
		<category: 'initialization'>
		default := 'Incorrect input.' .
		aSwitch := Dictionary new
	]
	"Description: Overrides Object's printOn message. This was overriden to make it easier to see the default
	expression of the switch."
	printOn: stream [
		<category: 'printing'>
		super printOn: stream .
		stream nextPutAll: ' with a default: ' .
		default printOn: stream
	]
	"Description: Changes the default statement to what the user specifies. This can be a block closure or other object."
	newDefault:aBlock [
		default := aBlock
	]
	"Description: This would be the switch portion of the Switch statement. Pass an expression to this message
		and it will return an object correlating to that expression or the default object if the former doesn't exist."
	evaluate:expression [
		^aSwitch at:expression ifAbsent:[default] 
	]
	"Description: This is used when there are multiple cases to be related to one set of statements. The first object passed should be
		a Set object, while the second is a block closure."
	newCases:cases attachedBlock:aBlock [
		cases do: [:each | aSwitch at:each put:aBlock]
	]
	"Description: This is used when there is only one case pertaining to a set of statements. The first object should be
		a constant object, like an Integer or Char,  and the second is a block closure."
	newCase:case attachedBlock:aBlock [
		aSwitch at:case put:aBlock
	]
	"Description: returns the default object."
	default [
		^default
	]
	"Description: returns the aSwitch object. This is really only for debugging purposes or if you need to see
		the cases."
	aSwitch [
		^aSwitch
	]
]
	
