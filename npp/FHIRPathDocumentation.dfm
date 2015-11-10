object FHIRPathDocumentationForm: TFHIRPathDocumentationForm
  Left = 0
  Top = 0
  Caption = 'FHIR Path Help'
  ClientHeight = 336
  ClientWidth = 527
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 298
    Width = 527
    Height = 38
    Align = alBottom
    TabOrder = 0
    object Button1: TButton
      Left = 442
      Top = 6
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Close'
      Default = True
      ModalResult = 8
      TabOrder = 0
    end
    object Button2: TButton
      Left = 8
      Top = 6
      Width = 93
      Height = 25
      Caption = 'Use Selection'
      TabOrder = 1
      OnClick = Button2Click
    end
  end
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 527
    Height = 298
    Align = alClient
    Lines.Strings = (
      'FHIR Path'
      ''
      
        'FHIRPath is a path based extraction language, somewhat like XPat' +
        'h. It is optimised to work best on FHIR '
      
        'resources. Operations are expressed in terms of the logical cont' +
        'ent of the resources, rather than their XML '
      
        'or JSON reprsentation. The expressions can (in theory) be conver' +
        't to XPath or JSON equivalents'
      ''
      
        'All FHIRPath operations result in a collection of Elements of va' +
        'rious types. When the expression begins '
      'evaluating, there is a collection on one element in focus.'
      ''
      '--------------------'
      '1. Usage'
      ''
      'FHIR Path is used in 3 places within the specification'
      
        '- search parameter paths - used to define what contents the para' +
        'meter refers to'
      
        '- slicing discriminator - used to indicate what elements define ' +
        'uniqueness'
      
        '- invariants in ElementDefinition, used to apply co-occurance an' +
        'd other rules to the contents'
      ''
      'implementations may find other uses for this as well'
      ''
      '--------------------'
      '2. Path selection'
      ''
      
        'The first fundamental operation is to select a set of elements b' +
        'y their path:'
      ''
      '  path.subPath.subPath - select all the elements on the path'
      ''
      'e.g. To select all the phone numbers for a patient'
      ''
      '  telecom.value'
      ''
      
        'when the focus is "Patient". Note that the path never includes t' +
        'he context - it is evaluated from the '
      'context.'
      ''
      'Special paths:'
      ' * - any child'
      ' ** any descendent'
      ' name* - recursive uses of the element'
      ' path.value[x].subPath - all kinds of value'
      ' path.valueQuantity.subPath - only values that are quantity'
      ''
      '-------------------------'
      '3. Boolean evaluations'
      ''
      
        'Collections can be evaluated as booleans in logical tests in cri' +
        'teria. When a collection is implicited converted '
      'to a boolean then:'
      ''
      '* if it has a single item that is a boolean:'
      '  - it has the value of the boolean'
      '* if it is empty'
      '  - it is false'
      '* else it is true'
      ''
      '-------------------------'
      '4. Functions'
      ''
      
        'In addition to selecting subelements, functions can be performed' +
        ' on the list. fucntions are names that are '
      'followed by a () with zero or more parameters.'
      ''
      'Note? Special paths:'
      '  $ = reset path to original root'
      ''
      'As an example:'
      ''
      '  telecom.where(use = '#39'home'#39').value.empty()'
      ''
      
        'This returns a collection of a single boolean element that conta' +
        'ins true if there is no home telephone '
      'numbers.'
      ''
      'The following operations are defined:'
      ''
      '.empty()'
      ''
      'true if the collection is empty'
      ''
      '.where(criteria)'
      ''
      
        'Filter the collection to only those elements that meet the state' +
        'd criteria expression. Expressions are '
      
        'evaluated with respect to the elements that meet the path. If th' +
        'e criteria is true, they are included in the '
      'result collection.'
      ''
      '.all(criteria)'
      ''
      
        'true if all items in the collection meet the criteria (also true' +
        ' if the collection is empty). The criteria is '
      'evaluated for each item in the collection'
      ''
      '.any(criteria)'
      ''
      
        'true if any items in the collection meet the criteria (and false' +
        ' if the collection is empty). The criteria is '
      'evaluated for each item in the collection'
      ''
      '.first()'
      ''
      'returns a collection containing the first item in the list '
      ''
      '.last()'
      ''
      'returns a collection containing the last item in the list '
      ''
      '.tail()'
      ''
      
        'returns a collection containing all but the last item in the lis' +
        't '
      ''
      '.count()'
      ''
      
        'returns a collection with a single value which is the integer co' +
        'unt of the collection'
      ''
      '.as-integer()'
      ''
      
        'converts a string to an integer (empty collection if it'#39's not a ' +
        'proper integer)'
      ''
      '.starts-with()'
      ''
      
        'filters the list to only include elements with a string value th' +
        'at starst with the specified content'
      ''
      '.length()'
      ''
      
        'returns the length of characters used to represent the value (pr' +
        'imitive types only) (does not include '
      'syntactical escapes). returns the longest item in the collection'
      ''
      '.matches(regex)'
      ''
      
        'returns a boolean for whether all the items in the collection ma' +
        'tch the given regex'
      ''
      '.distinct(path,path)'
      ''
      
        'returns true of all the elements in the list are distinct when u' +
        'sing the relative paths (simple paths only with '
      
        'no functions). If the elements in the list are primitives, this ' +
        'can be used with no paths (e.g. :distinct())'
      ''
      
        'Note: distinct may only be used in invariants and does not need ' +
        'to be implemented for other uses'
      ''
      '-----------------------------------'
      '5. operations'
      ''
      '  =, !=\, >, <, >=, <=  - normal use'
      '  in,'
      ''
      '-----------------------------------'
      '6. Fixed constants'
      ''
      '%sct - url for snomed ct'
      '%loinc - url for loinc'
      '%ucum - url for ucum'
      '%us-zip - regex [0-9]{5}(-[0-9]{4}){0,1}'
      '%vs-[name] - full url for the provided [name]'
      '%ext-[name]')
    TabOrder = 1
    OnClick = Memo1Click
    OnEnter = Memo1Enter
    OnExit = Memo1Exit
    OnKeyDown = Memo1KeyDown
    OnKeyUp = Memo1KeyUp
    OnMouseDown = Memo1MouseDown
    OnMouseLeave = Memo1MouseLeave
    OnMouseUp = Memo1MouseUp
  end
end
