Object subclass: #Mod10
	instanceVariableNames: 'value'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'user' !
   
!Mod10 methodsFor: 'assignment'!

value: anInteger
	" Assign anInteger as the value (asInteger) of a Mod10 object."

	value := anInteger rem: 10
! !

!Mod10 methodsFor: 'printing'!

printOn: stream
	value printOn: stream
! !

!Mod10 methodsFor: 'arithmetic'!

asInteger
   ^ value
!

+ aMod10
  | result |
  result := (Mod10 new) value: ( (value + aMod10 asInteger) rem: 10 ).
  ^ result
!

- aMod10
  ^ (Mod10 new) value: ( (value - aMod10 asInteger) rem: 10 )
!

* aMod10
  ^ (Mod10 new) value: ( (value * aMod10 asInteger) rem: 10 )
! !