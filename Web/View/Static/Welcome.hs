module Web.View.Static.Welcome where
import Web.View.Prelude
import qualified Data.Data as Data
import qualified Data.Dynamic as Dynamic
import qualified Control.Monad.State.Strict as State
import qualified Text.Blaze.Html5 as Blaze
import qualified Data.Maybe as Maybe
import qualified Data.List as List

data WelcomeView = WelcomeView

data Post
    = PlainPost { id :: UUID, title :: Text, body :: Text, public :: Bool }
    | MarkdownPost { id :: UUID, markdown :: Text, public :: Bool }
    deriving (Eq, Show, Data)

instance View WelcomeView where
    html WelcomeView = [hsx|
        <h2>Auto Form</h2>
        {autoFormFor formState}
    |]
        where
            formState :: [Post]
            formState = 
                [ PlainPost { id = def, title = "Hello World", body = "Test", public = True }
                , MarkdownPost { id = def, markdown = "# Hello World", public = False }
                ]


autoFormFor :: Data record => [record] -> Html
autoFormFor state = [hsx|
    <form method="GET" action="/">
        {forEach constructors (renderConstructor state)}
        <button type="submit" class="btn btn-primary">Save</button>
    </form>
|]
    where
        constructors = Data.dataTypeConstrs dataType
        dataType = Data.dataTypeOf (List.head state)

renderConstructor :: forall record. Data record => [record] -> Constr -> Html
renderConstructor state constructor = [hsx|
    <div class="form-check">
        <input class="form-check-input" type="radio" name="type" id={radioId} value={tshow constructor} checked>
        <fieldset class="form-group">
            <legend>
                <label for={radioId} class="form-check-label mb-0">
                    {constructor}
                </label>
            </legend>
            
            {fieldsHtml}
        </fieldset>
    </div>
|]
    where
        fields :: [String]
        fields = Data.constrFields constructor

        radioId :: Text
        radioId = tshow constructor <> "-radio"

        record :: record
        record = state
                |> find (\record -> toConstr record == constructor)
                |> fromMaybe (error $ "No state specified for " <> tshow constructor)

        fieldsHtml :: Blaze.Html
        fieldsHtml = gmapQ (mapConstructor dispatchField) record
                |> zip fields
                |> mapMaybe (\case
                        (fieldName, Just formField) -> Just (fieldName, formField)
                        (fieldName, Nothing) -> Nothing
                    )
                |> map (assignFieldName (tshow constructor))
                |> map toHtml
                |> mconcat

        mapConstructor :: Data d => [(d -> Maybe FormField)] -> d -> Maybe FormField
        mapConstructor [] _ = Nothing
        mapConstructor (f:fs) d = case f d of
            Just str -> Just str
            Nothing -> mapConstructor fs d

        dispatchField :: forall d. Data d => [(d -> Maybe FormField)]
        dispatchField =
            let ?formContext = formContext
            in
                [ \val -> (eqT :: Maybe (d :~: Text)) >>= \Refl -> Just (renderTextInput val)
                , \val -> (eqT :: Maybe (d :~: UUID)) >>= \Refl -> Just (renderTextInput val)
                , \val -> (eqT :: Maybe (d :~: Bool)) >>= \Refl -> Just (renderCheckboxInput val)
                ]

        formContext =
            FormContext
                { model = record
                , formAction = ""
                , cssFramework = theCSSFramework
                , formId = ""
                , formClass = "edit-form"
                , customFormAttributes = []
                , disableJavascriptSubmission = True
                }

assignFieldName :: Text -> (String, FormField) -> FormField
assignFieldName recordName (fieldName, formField) =
    formField
        { fieldName = recordName <> "_" <> cs fieldName
        , fieldLabel = fieldNameToFieldLabel (cs fieldName)
        , fieldInputId = cs (lcfirst recordName <> "_" <> cs fieldName)
        }

renderTextInput :: (?formContext :: FormContext record, InputValue value) => value -> FormField
renderTextInput value = FormField
        { fieldType = TextInput
        , fieldName = ""
        , fieldLabel = ""
        , fieldValue =  inputValue value
        , fieldInputId = ""
        , validatorResult = Nothing
        , fieldClass = ""
        , labelClass = ""
        , disabled = False
        , disableLabel = False
        , disableGroup = False
        , disableValidationResult = False
        , additionalAttributes = []
        , cssFramework = ?formContext.cssFramework
        , helpText = ""
        , placeholder = ""
        , required = False
        , autofocus = False
        }

renderCheckboxInput :: (?formContext :: FormContext record) => Bool -> FormField
renderCheckboxInput value = FormField
        { fieldType = CheckboxInput
        , fieldName = ""
        , fieldLabel = ""
        , fieldValue =  if value then "yes" else "no"
        , fieldInputId = ""
        , validatorResult = Nothing
        , fieldClass = ""
        , labelClass = ""
        , disabled = False
        , disableLabel = False
        , disableGroup = False
        , disableValidationResult = False
        , additionalAttributes = []
        , cssFramework = ?formContext.cssFramework
        , helpText = ""
        , placeholder = ""
        , required = False
        , autofocus = False
        }
