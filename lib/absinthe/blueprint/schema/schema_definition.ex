defmodule Absinthe.Blueprint.Schema.SchemaDefinition do
  @moduledoc false

  alias Absinthe.Blueprint

  defstruct description: nil,
            directive_artifacts: [],
            directive_definitions: [],
            directives: [],
            module: nil,
            source_location: nil,
            type_artifacts: [],
            type_definitions: [],
            type_extensions: [],
            # Added by phases
            flags: %{},
            imports: [],
            errors: [],
            __private__: [],
            __reference__: nil

  @type t :: %__MODULE__{
          description: nil | String.t(),
          # types: [Blueprint.Schema.FieldDefinition.t],
          directives: [Blueprint.Directive.t()],
          source_location: nil | Blueprint.SourceLocation.t(),
          # Added by phases
          flags: Blueprint.flags_t(),
          errors: [Absinthe.Phase.Error.t()]
        }
end
