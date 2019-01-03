defmodule Absinthe.Blueprint.Schema.FieldDefinition do
  @moduledoc false

  alias Absinthe.Blueprint

  @enforce_keys [:name]
  defstruct [
    :identifier,
    :module,
    :name,
    :type,
    arguments: [],
    complexity: nil,
    config: nil,
    default_value: nil,
    deprecation: nil,
    description: nil,
    directives: [],
    errors: [],
    flags: %{},
    function_ref: nil,
    middleware: [],
    source_location: nil,
    triggers: [],
    __reference__: nil,
    __private__: []
  ]

  @type t :: %__MODULE__{
          name: String.t(),
          identifier: atom,
          deprecation: nil | Blueprint.Schema.Deprecation.t(),
          arguments: [Blueprint.Schema.InputValueDefinition.t()],
          type: Blueprint.TypeReference.t(),
          directives: [Blueprint.Directive.t()],
          source_location: nil | Blueprint.SourceLocation.t(),
          # Added by DSL
          description: nil | String.t(),
          middleware: [any],
          # Added by phases
          flags: Blueprint.flags_t(),
          errors: [Absinthe.Phase.Error.t()]
        }

  @doc false
  def functions(), do: [:config, :complexity, :middleware, :triggers]
end
