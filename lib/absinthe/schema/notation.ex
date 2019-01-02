defmodule Absinthe.Schema.Notation do
  alias Absinthe.Blueprint.Schema
  alias Absinthe.Utils

  Module.register_attribute(__MODULE__, :placement, accumulate: true)

  defmacro __using__(_opts) do
    quote do
      import Absinthe.Schema.Notation, only: [schema: 1]
    end
  end

  ### Macro API ###

  defmacro schema(do: block) do
    quote location: :keep do
      Module.register_attribute(__MODULE__, :absinthe_blueprint, accumulate: true)
      Module.register_attribute(__MODULE__, :absinthe_desc, accumulate: true)
      Module.register_attribute(__MODULE__, :__absinthe_type_import__, accumulate: true)

      Module.register_attribute(__MODULE__, :__absinthe_enum_values__, accumulate: true)
      Module.register_attribute(__MODULE__, :__absinthe_locations__, accumulate: true)
      Module.register_attribute(__MODULE__, :__absinthe_directives__, accumulate: true)
      Module.register_attribute(__MODULE__, :__absinthe_sdl_definitions__, accumulate: true)

      @absinthe_blueprint %Absinthe.Blueprint{schema: __MODULE__}

      import Absinthe.Resolution.Helpers,
        only: [
          async: 1,
          async: 2,
          batch: 3,
          batch: 4
        ]

      import Absinthe.Schema.Notation, only: :macros
      unquote(block)
      import Absinthe.Schema.Notation, only: []
    end
  end

  @placement {:config, [under: [:field]]}
  @doc """
  Configure a subscription field.

  The returned topic can be single topic, or a list of topics

  ## Examples

  ```elixir
  config fn args, %{context: context} ->
    if authorized?(context) do
      {:ok, topic: args.client_id}
    else
      {:error, "unauthorized"}
    end
  end
  ```

  Alternatively can provide a list of topics:

  ```elixir
  config fn _, _ ->
    {:ok, topic: ["topic_one", "topic_two", "topic_three"]}
  end
  ```

  See `Absinthe.Schema.subscription/1` for details
  """
  defmacro config(config_fun) do
    quote do
      __config__(__MODULE__, unquote(config_fun))
    end
  end

  def __config__(module, func_ast) do
    put_attr(module, {:config, func_ast})
  end

  @placement {:trigger, [under: [:field]]}
  @doc """
  Set a trigger for a subscription field.

  It accepts one or more mutation field names, and can be called more than once.

  ```
  mutation do
    field :gps_event, :gps_event
    field :user_checkin, :user
  end
  subscription do
    field :location_update, :user do
      arg :user_id, non_null(:id)

      config fn args, _ ->
        {:ok, topic: args.user_id}
      end

      trigger :gps_event, topic: fn event ->
        event.user_id
      end

      trigger :user_checkin, topic: fn user ->
        [user.id, user.parent_id]
      end
    end
  end
  ```

  Trigger functions are only called once per event, so database calls within
  them do not present a significant burden.

  See the `subscription/2` macro docs for additional details
  """
  defmacro trigger(mutations, attrs) do
    quote do
      Absinthe.Schema.Notation.__trigger__(
        __MODULE__,
        unquote(mutations),
        unquote(attrs)
      )
    end
  end

  def __trigger__(module, mutations, attrs) do
    mutations
    |> List.wrap()
    |> Enum.each(&put_attr(module, {:trigger, {&1, attrs}}))
  end

  # OBJECT

  @placement {:object, [toplevel: true]}
  @doc """
  Define an object type.

  Adds an `Absinthe.Type.Object` to your schema.

  ## Placement

  #{Utils.placement_docs(@placement)}

  ## Examples

  Basic definition:

  ```
  object :car do
    # ...
  end
  ```

  Providing a custom name:

  ```
  object :car, name: "CarType" do
    # ...
  end
  ```
  """
  @reserved_identifiers ~w(query mutation subscription)a
  defmacro object(identifier, attrs \\ [], block)

  defmacro object(identifier, attrs, do: block) do
    {attrs, block} =
      case Keyword.pop(attrs, :meta) do
        {nil, attrs} ->
          {attrs, block}

        {meta, attrs} ->
          meta_ast =
            quote do
              meta(unquote(meta))
            end

          block = [meta_ast, block]
          {attrs, block}
      end

    Absinthe.Schema.Notation.record!(
      __CALLER__,
      Schema.ObjectTypeDefinition,
      identifier,
      attrs,
      block
    )

    quote do
      @__current_absinthe_object__ unquote(identifier)
      @__absinthe_fields__ []
      @__absinthe_interfaces__ []
      unquote(block)

      Absinthe.Schema.Notation.__object__(
        __MODULE__,
        __ENV__.file,
        __ENV__.line,
        unquote(identifier),
        unquote(attrs),
        @__absinthe_fields__,
        @__absinthe_interfaces__
      )
    end
  end

  def __object__(module, identifier, attrs, fields, interfaces) do
    name = attrs[:name] || default_name(Schema.ObjectTypeDefinition, identifier)

    object = %Schema.ObjectTypeDefinition{
      description: attrs[:description],
      # directives: directives,
      fields: fields,
      identifier: identifier,
      interfaces: interfaces,
      is_type_of: attrs[:is_type_of],
      name: name
    }

    Module.put_attribute(module, :__absinthe_objects__, object)
  end

  @placement {:interface, [toplevel: true]}
  @doc """
  Define an interface type.

  Adds an `Absinthe.Type.Interface` to your schema.

  Also see `interface/1` and `interfaces/1`, which declare
  that an object implements one or more interfaces.

  ## Placement

  #{Utils.placement_docs(@placement)}

  ## Examples

  ```
  interface :vehicle do
    field :wheel_count, :integer
  end

  object :rally_car do
    field :wheel_count, :integer
    interface :vehicle
  end
  ```
  """
  defmacro interface(identifier, attrs \\ [], do: block) do
    quote do
      # |> record!(Schema.InterfaceTypeDefinition, identifier, attrs)
      unquote(block)

      Absinthe.Schema.Notation.__interface__(
        __MODULE__,
        __ENV__.file,
        __ENV__.line,
        unquote(identifier),
        unquote(attrs)
      )
    end
  end

  @doc """
  Declare an implemented interface for an object.

  Adds an `Absinthe.Type.Interface` to your schema.

  See also `interfaces/1`, which can be used for multiple interfaces,
  and `interface/3`, used to define interfaces themselves.

  ## Examples

  ```
  object :car do
    interface :vehicle
    # ...
  end
  ```
  """
  @placement {:interface_attribute, [under: :object]}
  defmacro interface(identifier) do
    quote do
      interface unquote(identifier) do
        :ok
      end
    end
  end

  def __interface__(module, identifier) do
    put_attr(module, {:interface, identifier})
  end

  @placement {:interfaces, [under: :object]}
  @doc """
  Declare implemented interfaces for an object.

  See also `interface/1`, which can be used for one interface,
  and `interface/3`, used to define interfaces themselves.

  ## Placement

  #{Utils.placement_docs(@placement)}

  ## Examples

  ```
  object :car do
    interfaces [:vehicle, :branded]
    # ...
  end
  ```
  """
  defmacro interfaces(interfaces) when is_list(interfaces) do
    quote do
      for interface <- unquote(interfaces) do
        interface interface
      end
    end
  end

  @placement {:resolve, [under: [:field]]}
  @doc """
  Mark a field as deprecated

  In most cases you can simply pass the deprecate: "message" attribute. However
  when using the block form of a field it can be nice to also use this macro.

  ## Placement

  #{Utils.placement_docs(@placement)}

  ## Examples
  ```
  field :foo, :string do
    deprecate "Foo will no longer be supported"
  end
  ```

  This is how to deprecate other things
  ```
  field :foo, :string do
    arg :bar, :integer, deprecate: "This isn't supported either"
  end

  enum :colors do
    value :red
    value :blue, deprecate: "This isn't supported"
  end
  ```
  """
  defmacro deprecate(msg) do
    quote do
      Absinthe.Schema.Notation.__deprecate__(__MODULE__, unquote(msg))
    end
  end

  @doc false
  # Record a deprecation in the current scope
  def __deprecate__(module, msg) do
    deprecation = build_deprecation(msg)
    Module.put_attribute(module, :deprecation, deprecation)
  end

  # INTERFACES

  @placement {:resolve_type, [under: [:interface, :union]]}
  @doc """
  Define a type resolver for a union or interface.

  See also:
  * `Absinthe.Type.Interface`
  * `Absinthe.Type.Union`

  ## Placement

  #{Utils.placement_docs(@placement)}

  ## Examples

  ```
  interface :entity do
    # ...
    resolve_type fn
      %{employee_count: _},  _ ->
        :business
      %{age: _}, _ ->
        :person
    end
  end
  ```
  """
  defmacro resolve_type(func_ast) do
    quote do
      Absinthe.Schema.Notation.__resolve_type__(__MODULE__, unquote(func_ast))
    end
  end

  def __resolve_type__(module, function_ast) do
    put_attr(module, {:resolve_type, function_ast})
  end

  defp handle_deprecate(attrs) do
    deprecation = build_deprecation(attrs[:deprecate])

    attrs
    |> Keyword.delete(:deprecate)
    |> Keyword.put(:deprecation, deprecation)
  end

  defp build_deprecation(true) do
    %Absinthe.Type.Deprecation{reason: nil}
  end

  defp build_deprecation(reason) when is_binary(reason) do
    %Absinthe.Type.Deprecation{reason: reason}
  end

  defp build_deprecation(_), do: nil

  # FIELDS
  @placement {:field, [under: [:input_object, :interface, :object]]}
  @doc """
  Defines a GraphQL field

  See `field/4`
  """

  defmacro field(identifier, attrs) when is_list(attrs) do
    quote do
      field unquote(identifier), unquote(attrs) do
        :ok
      end
    end
  end

  defmacro field(identifier, type) do
    quote do
      field unquote(identifier), type: unquote(type) do
        :ok
      end
    end
  end

  @doc """
  Defines a GraphQL field

  See `field/4`
  """
  defmacro field(identifier, attrs \\ [], block)
  defmacro field(identifier, attrs, do: block) when is_list(attrs) do
    quote do
      for {arg_identifier, arg_attrs} <- Keyword.get(unquote(attrs), :args, []) do
        arg arg_identifier, arg_attrs
      end

      unquote(block)

      resolve = Keyword.get(unquote(attrs), :resolve)
      if resolve do
        resolve resolve
      end

      Absinthe.Schema.Notation.__field__(
        __MODULE__,
        __ENV__.file,
        __ENV__.line,
        unquote(identifier),
        unquote(attrs)
      )
    end
  end

  defmacro field(identifier, type, do: block) do
    quote do
      field unquote(identifier), type: unquote(type) do
        unquote(block)
      end
    end
  end

  defmacro field(identifier, type, attrs) do
    quote do
      field unquote(identifier), Keyword.put(unquote(attrs), :type, unquote(type)) do
        :ok
      end
    end
  end

  @doc """
  Defines a GraphQL field.

  ## Placement

  #{Utils.placement_docs(@placement)}

  `query`, `mutation`, and `subscription` are
  all objects under the covers, and thus you'll find `field` definitions under
  those as well.

  ## Examples
  ```
  field :id, :id
  field :age, :integer, description: "How old the item is"
  field :name, :string do
    description "The name of the item"
  end
  field :location, type: :location
  ```
  """
  defmacro field(identifier, type, attrs, do: block) do
    quote do
      field unquote(identifier), Keyword.put(unquote(attrs), :type, unquote(type)) do
        unquote(block)
      end
    end
  end

  def __field__(module, file, line, identifier, attrs) do
    reference = build_reference(module, file, line)

    attrs
    |> Keyword.delete(:args)
    |> handle_deprecate
    |> Keyword.put(:__reference__, reference)
    |> Keyword.put(:identifier, identifier)
    |> Keyword.put(:module, module)

    field = struct!(Schema.FieldDefinition, attrs)
    Module.put_attribute(module, :__absinthe_fields__, field)
  end

  @placement {:resolve, [under: [:field]]}
  @doc """
  Defines a resolve function for a field

  Specify a 2 or 3 arity function to call when resolving a field.

  You can either hard code a particular anonymous function, or have a function
  call that returns a 2 or 3 arity anonymous function. See examples for more information.

  Note that when using a hard coded anonymous function, the function will not
  capture local variables.

  ### 3 Arity Functions

  The first argument to the function is the parent entity.
  ```
  {
    user(id: 1) {
      name
    }
  }
  ```
  A resolution function on the `name` field would have the result of the `user(id: 1)` field
  as its first argument. Top level fields have the `root_value` as their first argument.
  Unless otherwise specified, this defaults to an empty map.

  The second argument to the resolution function is the field arguments. The final
  argument is an `Absinthe.Resolution` struct, which includes information like
  the `context` and other execution data.

  ### 2 Arity Function

  Exactly the same as the 3 arity version, but without the first argument (the parent entity)

  ## Placement

  #{Utils.placement_docs(@placement)}

  ## Examples
  ```
  query do
    field :person, :person do
      resolve &Person.resolve/2
    end
  end
  ```

  ```
  query do
    field :person, :person do
      resolve fn %{id: id}, _ ->
        {:ok, Person.find(id)}
      end
    end
  end
  ```

  ```
  query do
    field :person, :person do
      resolve lookup(:person)
    end
  end

  def lookup(:person) do
    fn %{id: id}, _ ->
      {:ok, Person.find(id)}
    end
  end
  ```
  """
  defmacro resolve(func_ast) do
    quote do
      middleware Absinthe.Resolution, unquote(func_ast)
    end
  end

  @placement {:complexity, [under: [:field]]}
  defmacro complexity(complexity) do
    quote do
      Absinthe.Schema.Notation.__complexity__(__MODULE__, unquote(complexity))
    end
  end

  @doc false
  def __complexity__(module, complexity) do
    Module.put_attribute(module, :__absinthe_complexity__, complexity)
  end

  @placement {:middleware, [under: [:field]]}
  defmacro middleware(new_middleware, opts \\ []) do
    __CALLER__
    |> record_middleware!(new_middleware, opts)

    quote do
    end
  end

  @placement {:is_type_of, [under: [:object]]}
  @doc """

  ## Placement

  #{Utils.placement_docs(@placement)}
  """
  defmacro is_type_of(func_ast) do
    quote do
      __is_type_of__(__MODULE__, unquote(func_ast))
    end
  end

  @doc false
  # Record a type checker in the current scope
  def __is_type_of__(module, func_ast) do
    put_attr(module, {:is_type_of, func_ast})
  end

  @placement {:arg, [under: [:directive, :field]]}
  # ARGS
  @doc """
  Add an argument.

  ## Placement

  #{Utils.placement_docs(@placement)}

  ## Examples

  ```
  field do
    arg :size, :integer
    arg :name, :string, description: "The desired name"
  end
  ```
  """
  defmacro arg(identifier, type, attrs) do
    quote location: :keep do
      arg unquote(identifier), Keyword.put(unquote(attrs), :type, unquote(type))
    end
  end

  @doc """
  Add an argument.

  See `arg/3`
  """
  defmacro arg(identifier, attrs) when is_list(attrs) do
    quote location: :keep do
      Absinthe.Schema.Notation.__arg__(
        __MODULE__,
        __ENV__.file,
        __ENV__.line,
        unquote(identifier),
        unquote(attrs)
      )
    end
  end

  defmacro arg(identifier, type) do
    quote location: :keep do
      arg unquote(identifier), type: unquote(type)
    end
  end

  def __arg__(module, file, line, identifier, attrs) do
    reference = build_reference(module, file, line)

    attrs =
      attrs
      |> handle_deprecate()
      |> Keyword.put(:identifier, identifier)
      |> Keyword.put(:module, module)
      |> Keyword.put(:name, to_string(identifier))
      |> Keyword.put(:__reference__, reference)

    arg = struct!(Schema.InputValueDefinition, attrs)
    Module.put_attribute(module, :__absinthe_args__, arg)
  end

  # SCALARS

  @placement {:scalar, [toplevel: true]}
  @doc """
  Define a scalar type

  A scalar type requires `parse/1` and `serialize/1` functions.

  ## Placement

  #{Utils.placement_docs(@placement)}

  ## Examples
  ```
  scalar :time, description: "ISOz time" do
    parse &Timex.parse(&1.value, "{ISOz}")
    serialize &Timex.format!(&1, "{ISOz}")
  end
  ```
  """
  defmacro scalar(identifier, attrs \\ [], do: block) do
    quote do
      unquote(block)

      record!(
        __MODULE__,
        Schema.ScalarTypeDefinition,
        unquote(identifier),
        unquote(attrs)
      )
    end
  end

  @doc """
  Defines a scalar type

  See `scalar/3`
  """
  defmacro scalar(identifier, attrs) do
    quote do
      scalar unquote(identifier), unquote(attrs) do
        :ok
      end
    end
  end

  @placement {:serialize, [under: [:scalar]]}
  @doc """
  Defines a serialization function for a `scalar` type

  The specified `serialize` function is used on outgoing data. It should simply
  return the desired external representation.

  ## Placement

  #{Utils.placement_docs(@placement)}
  """
  defmacro serialize(func_ast) do
    quote do
      Absinthe.Schema.Notation.__serialize__(__MODULE__, unquote(func_ast))
    end
  end

  @doc false
  # Record a serialize function in the current scope
  def __serialize__(module, fun_ast) do
    put_attr(module, {:serialize, fun_ast})
  end

  @placement {:private,
              [under: [:field, :object, :input_object, :enum, :scalar, :interface, :union]]}
  @doc false
  defmacro private(owner, key, value) do
    quote do
      Absinthe.Schema.Notation.__private__(__MODULE__, unquote(owner), [
        {unquote(key), unquote(value)}
      ])
    end
  end

  @placement {:meta,
              [under: [:field, :object, :input_object, :enum, :scalar, :interface, :union]]}
  @doc """
  Defines a metadata key/value pair for a custom type.

  For more info see `meta/1`

  ### Examples

  ```
  meta :cache, false
  ```

  ## Placement

  #{Utils.placement_docs(@placement)}
  """
  defmacro meta(key, value) do
    quote do
      meta [{unquote(key), unquote(value)}]
    end
  end

  @doc """
  Defines list of metadata's key/value pair for a custom type.

  This is generally used to facilitate libraries that want to augment Absinthe
  functionality

  ## Examples

  ```
  object :user do
    meta cache: true, ttl: 22_000
  end

  object :user, meta: [cache: true, ttl: 22_000] do
    # ...
  end
  ```

  The meta can be accessed via the `Absinthe.Type.meta/2` function.

  ```
  user_type = Absinthe.Schema.lookup_type(MyApp.Schema, :user)

  Absinthe.Type.meta(user_type, :cache)
  #=> true

  Absinthe.Type.meta(user_type)
  #=> [cache: true, ttl: 22_000]
  ```

  ## Placement

  #{Utils.placement_docs(@placement)}
  """
  defmacro meta(keyword_list) do
    quote do
      Absinthe.Schema.Notation.__private__(__MODULE__, :meta, unquote(keyword_list))
    end
  end

  @placement {:parse, [under: [:scalar]]}
  @doc """
  Defines a parse function for a `scalar` type

  The specified `parse` function is used on incoming data to transform it into
  an elixir datastructure.

  It should return `{:ok, value}` or `:error`

  ## Placement

  #{Utils.placement_docs(@placement)}
  """
  defmacro parse(func_ast) do
    quote do
      Absinthe.Schema.Notation.__parse__(__MODULE__, unquote(func_ast))
    end
  end

  @doc false
  # Record a parse function in the current scope
  def __parse__(module, fun_ast) do
    put_attr(module, {:parse, fun_ast})
  end

  # DIRECTIVES

  @placement {:directive, [toplevel: true]}
  @doc """
  Defines a directive

  ## Placement

  #{Utils.placement_docs(@placement)}

  ## Examples

  ```
  directive :mydirective do

    arg :if, non_null(:boolean), description: "Skipped when true."

    on [:field, :fragment_spread, :inline_fragment]

    expand fn
      %{if: true}, node ->
        Blueprint.put_flag(node, :skip, __MODULE__)
      _, node ->
        node
    end

  end
  ```
  """
  defmacro directive(identifier, attrs \\ [], do: block) do
    quote do
      Module.delete_attribute(__MODULE__, :__absinthe_args__)
      Module.delete_attribute(__MODULE__, :__absinthe_locations__)
      @__absinthe_expand__ nil

      @desc nil

      unquote(block)

      if @desc do
        @absinthe_desc @desc
      end

      Absinthe.Schema.Notation.__directive__(
        __MODULE__,
        __ENV__.file,
        __ENV__.line,
        unquote(identifier),
        unquote(attrs),
        @__absinthe_args__,
        @__absinthe_locations__,
        @__absinthe_expand__
      )
    end
  end

  @doc false
  # Record a directive
  def __directive__(module, file, line, identifier, attrs, args, locations, expand) do
    reference = build_reference(module, file, line)

    attrs =
      attrs
      |> Keyword.put(:args, args)
      |> Keyword.put(:expand, expand)
      |> Keyword.put(:identifier, identifier)
      |> Keyword.put(:locations, locations)
      |> Keyword.put(:__reference__, reference)
      |> Keyword.put_new(:name, to_string(identifier))

    directive = struct!(Schema.DirectiveDefinition, attrs)
    Module.put_attribute(module, :__absinthe_directives__, directive)
  end

  @placement {:on, [under: :directive]}
  @doc """
  Declare a directive as operating an a AST node type

  See `directive/2`

  ## Placement

  #{Utils.placement_docs(@placement)}
  """
  defmacro on(ast_node) do
    quote do
      Absinthe.Schema.Notation.__locations__(module, unquote(ast_node))
    end
  end

  @doc false
  # Record directive AST nodes in the current scope
  def __locations__(module, locations) do
    locations
    |> List.wrap()
    |> Enum.each(&Module.put_attribute(module, :__absinthe_directive_locations__, &1))
  end

  @placement {:expand, [under: :directive]}
  @doc """
  Define the expansion for a directive

  ## Placement

  #{Utils.placement_docs(@placement)}
  """
  defmacro expand(func_ast) do
    quote do
      Absinthe.Schema.Notation.__expand__(__MODULE__, unquote(func_ast))
    end
  end

  def __expand__(module, func_ast) do
    Module.put_attribute(module, :__absinthe_expand__, func_ast)
  end

  # INPUT OBJECTS

  @placement {:input_object, [toplevel: true]}
  @doc """
  Defines an input object

  See `Absinthe.Type.InputObject`

  ## Placement

  #{Utils.placement_docs(@placement)}

  ## Examples
  ```
  input_object :contact_input do
    field :email, non_null(:string)
  end
  ```
  """
  defmacro input_object(identifier, attrs \\ [], do: block) do
    quote do
      unquote(block)

      record!(
        __MODULE__,
        Schema.InputObjectTypeDefinition,
        unquote(identifier),
        unquote(attrs)
      )
    end
  end

  # UNIONS

  @placement {:union, [toplevel: true]}
  @doc """
  Defines a union type

  See `Absinthe.Type.Union`

  ## Placement

  #{Utils.placement_docs(@placement)}

  ## Examples
  ```
  union :search_result do
    description "A search result"

    types [:person, :business]
    resolve_type fn
      %Person{}, _ -> :person
      %Business{}, _ -> :business
    end
  end
  ```
  """
  defmacro union(identifier, attrs \\ [], do: block) do
    quote do
      Module.delete_attribute(__MODULE__, :__absinthe_union_types__)
      @__absinthe_resolve_type__ nil

      @desc nil

      unquote(block)

      if @desc do
        @absinthe_desc @desc
      end

      Absinthe.Schema.Notation.__union__(
        __MODULE__,
        __ENV__.file,
        __ENV__.line,
        unquote(identifier),
        unquote(attrs),
        @desc,
        @__absinthe_union_types__,
        @__absinthe_resolve_type__
      )
    end
  end

  def __union__(module, file, line, identifier, attrs, description, types, resolve_type) do
    reference = build_reference(module, file, line)

    attrs =
      attrs
      |> Keyword.put(:__reference__, reference)
      |> Keyword.put(:description, description)
      |> Keyword.put(:identifier, identifier)
      |> Keyword.put(:types, types)
      |> Keyword.put(:resolve_type, resolve_type)

    union = struct!(Absinthe.Type.Union, attrs)
    Module.put_attribute(module, :absinthe_desc, union)
  end

  @placement {:types, [under: [:union]]}
  @doc """
  Defines the types possible under a union type

  See `union/3`

  ## Placement

  #{Utils.placement_docs(@placement)}
  """
  defmacro types(types) do
    quote do
      Absinthe.Schema.Notation.__types__(__MODULE__, unquote(types))
    end
  end

  def __types__(module, types) do
    Module.put_attribute(module, :__absinthe_union_types__, types)
  end

  # ENUMS

  @placement {:enum, [toplevel: true]}
  @doc """
  Defines an enum type

  ## Placement

  #{Utils.placement_docs(@placement)}

  ## Examples

  Handling `RED`, `GREEN`, `BLUE` values from the query document:

  ```
  enum :color do
    value :red
    value :green
    value :blue
  end
  ```

  A given query document might look like:

  ```graphql
  {
    foo(color: RED)
  }
  ```

  Internally you would get an argument in elixir that looks like:

  ```elixir
  %{color: :red}
  ```

  If your return value is an enum, it will get serialized out as:

  ```json
  {"color": "RED"}
  ```

  You can provide custom value mappings. Here we use `r`, `g`, `b` values:

  ```
  enum :color do
    value :red, as: "r"
    value :green, as: "g"
    value :blue, as: "b"
  end
  ```

  """
  defmacro enum(identifier, attrs \\ [], do: block) do
    quote do
      Module.delete_attribute(__MODULE__, :__absinthe_enum_values__)
      Module.delete_attribute(__MODULE__, :__absinthe_enum_directives__)

      unquote(block)

      Absinthe.Schema.Notation.__enum__(
        __MODULE__,
        __ENV__.file,
        __ENV__.line,
        unquote(identifier),
        unquote(attrs),
        @__absinthe_enum_values__,
        @__absinthe_enum_directives__
      )
    end
  end

  @doc """
  Defines an enum type

  See `enum/3`
  """
  defmacro enum(identifier, attrs) do
    quote do
      enum unquote(identifier), Keyword.delete(unquote(attrs), :values) do
        for attr <- Keyword.get(unquote(attrs), :values) do
          value attr
        end
      end
    end
  end

  def __enum__(module, file, line, identifier, attrs, values, directives) do
    reference = build_reference(module, file, line)

    enum = %Schema.EnumValueDefinition{
      __reference__: reference,
      description: attrs[:description],
      directives: directives,
      identifier: identifier,
      module: module,
      values: Enum.into(values, %{}, {&Map.get(&1, :identifier), &1}),
      values_by_internal_value: Enum.into(values, %{}, {&Map.get(&1, :value), &1}),
      values_by_name: Enum.into(values, %{}, {&Map.get(&1, :name), &1})
    }

    Module.put_attribute(module, :__absinthe_enums__, enum)
  end

  defmacro values(values) do
    quote do
      for val <- unquote(values) do
        value val
      end
    end
  end

  @placement {:value, [under: [:enum]]}
  @doc """
  Defines a value possible under an enum type

  See `enum/3`

  ## Placement

  #{Utils.placement_docs(@placement)}
  """
  defmacro value(identifier, raw_attrs \\ []) do
    quote do
      Absinthe.Schema.Notation.__value__(
        __MODULE__,
        __ENV__.file,
        __ENV__.line,
        unquote(identifier),
        unquote(raw_attrs)
      )
    end
  end

  @doc false
  def __value__(module, file, line, identifier, raw_attrs) do
    reference = build_reference(module, file, line)

    name =
      identifier
      |> to_string()
      |> String.upcase()

    attrs =
      raw_attrs
      |> Keyword.put(:__reference__, reference)
      |> Keyword.put(:identifier, identifier)
      |> Keyword.put(:module, module)
      |> Keyword.put(:value, Keyword.get(raw_attrs, :as, identifier))
      |> Keyword.put_new(:name, name)
      |> Keyword.delete(:as)
      |> handle_deprecate()

    value = struct!(Schema.EnumValueDefinition, attrs)

    Module.put_attribute(module, :__absinthe_enum_values__, value)
  end

  # GENERAL ATTRIBUTES

  @placement {:description, [toplevel: false]}
  @doc """
  Defines a description

  This macro adds a description to any other macro which takes a block.

  Note that you can also specify a description by using `@desc` above any item
  that can take a description attribute.

  ## Placement

  #{Utils.placement_docs(@placement)}
  """
  defmacro description(text) do
    quote do
      Absinthe.Schema.Notation.__description__(__MODULE__, unquote(text))
    end
  end

  @doc false
  def __description__(module, text) do
    description = String.trim(text)
    Module.put_attribute(module, :desc, description)
  end

  # TYPE UTILITIES
  @doc """
  Marks a type reference as non null

  See `field/3` for examples
  """
  defmacro non_null(type) do
    quote do
      %Absinthe.Blueprint.TypeReference.NonNull{of_type: unquote(type)}
    end
  end

  @doc """
  Marks a type reference as a list of the given type

  See `field/3` for examples
  """
  defmacro list_of(type) do
    quote do
      %Absinthe.Blueprint.TypeReference.List{of_type: unquote(type)}
    end
  end

  @placement {:import_fields, [under: [:input_object, :interface, :object]]}
  @doc """
  Import fields from another object

  ## Example
  ```
  object :news_queries do
    field :all_links, list_of(:link)
    field :main_story, :link
  end

  object :admin_queries do
    field :users, list_of(:user)
    field :pending_posts, list_of(:post)
  end

  query do
    import_fields :news_queries
    import_fields :admin_queries
  end
  ```

  Import fields can also be used on objects created inside other modules that you
  have used import_types on.

  ```
  defmodule MyApp.Schema.NewsTypes do
    use Absinthe.Schema.Notation

    object :news_queries do
      field :all_links, list_of(:link)
      field :main_story, :link
    end
  end
  defmodule MyApp.Schema.Schema do
    use Absinthe.Schema

    import_types MyApp.Schema.NewsTypes

    query do
      import_fields :news_queries
      # ...
    end
  end
  ```
  """
  defmacro import_fields(source_criteria, opts \\ []) do
    quote do
      Absinthe.Schema.Notation.__import_fields__(
        __MODULE__,
        unquote(source_criteria),
        unquote(opts)
      )
    end
  end

  @doc false
  def __import_fields__(module, source_criteria, opts) do
    put_attr(module, {:import_fields, {source_criteria, opts}})
  end

  @placement {:import_types, [toplevel: true]}
  @doc """
  Import types from another module

  Very frequently your schema module will simply have the `query` and `mutation`
  blocks, and you'll want to break out your other types into other modules. This
  macro imports those types for use the current module

  ## Placement

  #{Utils.placement_docs(@placement)}

  ## Examples
  ```
  import_types MyApp.Schema.Types

  import_types MyApp.Schema.Types.{TypesA, TypesB}
  ```
  """
  defmacro import_types(type_module_ast, opts \\ []) do
    quote do
      __import_types__(__MODULE__, unquote(type_module), unquote(opts))
    end
  end

  @doc false
  defp __import_types__(module, type_module, opts) do
    Module.put_attribute(module, :__absinthe_type_imports__, {type_module, opts})
  end

  @placement {:import_sdl, [toplevel: true]}
  @type import_sdl_option :: {:path, String.t() | Macro.t()}
  @doc """
  Import types defined using the Schema Definition Language (SDL).

  TODO: Explain handlers

  ## Placement

  #{Utils.placement_docs(@placement)}

  ## Examples

  Directly embedded SDL:

  ```
  import_sdl \"""
  type Query {
    posts: [Post]
  }

  type Post {
    title: String!
    body: String!
  }
  \"""
  ```

  Loaded from a file location (supporting recompilation on change):

  ```
  import_sdl path: "/path/to/sdl.graphql"
  ```

  TODO: Example for dynamic loading during init
  """
  @spec import_sdl([import_sdl_option(), ...]) :: Macro.t()
  defmacro import_sdl(opts) when is_list(opts) do
    __CALLER__
    |> do_import_sdl(nil, opts)
  end

  @spec import_sdl(String.t() | Macro.t(), [import_sdl_option()]) :: Macro.t()
  defmacro import_sdl(sdl, opts \\ []) do
    __CALLER__
    |> do_import_sdl(sdl, opts)
  end

  ### Recorders ###
  #################

  @scoped_types [
    Schema.ObjectTypeDefinition,
    Schema.FieldDefinition,
    Schema.ScalarTypeDefinition,
    Schema.InputObjectTypeDefinition,
    Schema.UnionTypeDefinition,
    Schema.InterfaceTypeDefinition,
    Schema.DirectiveDefinition
  ]

  # Record private values
  def __private__(module, owner, keyword_list) when is_list(keyword_list) do
    put_attr(module, {:__private__, [{owner, keyword_list}]})
  end

  def record_middleware!(env, new_middleware, opts) do
    new_middleware =
      case expand_ast(new_middleware, env) do
        {module, fun} ->
          {:{}, [], [{module, fun}, opts]}

        atom when is_atom(atom) ->
          case Atom.to_string(atom) do
            "Elixir." <> _ ->
              {:{}, [], [{atom, :call}, opts]}

            _ ->
              {:{}, [], [{env.module, atom}, opts]}
          end

        val ->
          val
      end

    put_attr(env.module, {:middleware, [new_middleware]})
  end

  # ------------------------------

  def build_reference(module, file, line) do
    %{
      module: module,
      location: %{
        file: file,
        line: line
      }
    }
  end

  defp scoped_def(caller, type, identifier, attrs, body) do
    attrs =
      attrs
      |> Keyword.put(:identifier, identifier)
      |> Keyword.put_new(:name, default_name(type, identifier))
      |> Keyword.put(:module, module)

    definition = struct!(type, attrs)

    ref = put_attr(module, definition)

    quote do
      unquote(body)
    end
  end

  defp default_name(Schema.FieldDefinition, identifier) do
    Atom.to_string(identifier)
  end

  defp default_name(_, identifier) do
    identifier
    |> Atom.to_string()
    |> Absinthe.Utils.camelize()
  end

  @spec do_import_sdl(Macro.Env.t(), nil, [import_sdl_option()]) :: Macro.t()
  defp do_import_sdl(env, nil, opts) do
    case Keyword.fetch(opts, :path) do
      {:ok, path} ->
        quote do
          @__absinthe_import_sdl_path__ unquote(path)
          File.read!(@__absinthe_import_sdl_path__)
          @external_resource @__absinthe_import_sdl_path__
        end

      :error ->
        raise Absinthe.Schema.Notation.Error,
              "Must provide `:path` option to `import_sdl` unless passing a raw SDL string as the first argument"
    end
  end

  @spec do_import_sdl(Macro.Env.t(), String.t() | Macro.t(), Keyword.t()) :: Macro.t()
  defp do_import_sdl(env, sdl, opts) do
    ref = build_reference(env)

    quote do
      with {:ok, definitions} <-
             Absinthe.Schema.Notation.SDL.parse(
               unquote(sdl),
               __MODULE__,
               unquote(Macro.escape(ref)),
               unquote(Macro.escape(opts))
             ) do
        for definition <- definitions do
          @__absinthe_sdl_definitions__ definition
        end
      else
        {:error, error} ->
          raise Absinthe.Schema.Notation.Error, "`import_sdl` could not parse SDL:\n#{error}"
      end
    end
  end

  defmacro __before_compile__(env) do
    module_attribute_descs =
      env.module
      |> Module.get_attribute(:absinthe_desc)
      |> Map.new()

    attrs =
      env.module
      |> Module.get_attribute(:absinthe_blueprint)

    imports =
      (Module.get_attribute(env.module, :__absinthe_type_imports__) || [])
      |> Enum.uniq()
      |> Enum.map(fn
        module when is_atom(module) -> {module, []}
        other -> other
      end)

    schema_def = %Schema.SchemaDefinition{
      imports: imports,
      module: env.module,
      __reference__: %{
        location: %{file: env.file, line: 0}
      }
    }

    blueprint =
      attrs
      |> List.insert_at(1, schema_def)
      |> Absinthe.Blueprint.Schema.build()

    # TODO: handle multiple schemas
    [schema] = blueprint.schema_definitions

    {schema, functions} = lift_functions(schema, env.module)

    sdl_definitions =
      (Module.get_attribute(env.module, :__absinthe_sdl_definitions__) || [])
      |> List.flatten()
      |> Enum.map(fn type_definition ->
        Absinthe.Blueprint.prewalk(type_definition, fn
          %{module: _} = node ->
            %{node | module: env.module}

          node ->
            node
        end)
      end)

    schema = Map.update!(schema, :type_definitions, &(sdl_definitions ++ &1))

    blueprint = %{blueprint | schema_definitions: [schema]}

    quote do
      def __absinthe_blueprint__ do
        unquote(Macro.escape(blueprint, unquote: true))
      end

      unquote_splicing(functions)
    end
  end

  def lift_functions(schema, origin) do
    Absinthe.Blueprint.prewalk(schema, [], &lift_functions(&1, &2, origin))
  end

  def lift_functions(node, acc, origin) do
    {node, ast} = functions_for_type(node, origin)
    {node, ast ++ acc}
  end

  defp functions_for_type(%Schema.FieldDefinition{} = type, origin) do
    grab_functions(
      origin,
      type,
      {Schema.FieldDefinition, type.function_ref},
      Schema.functions(Schema.FieldDefinition)
    )
  end

  defp functions_for_type(%module{identifier: identifier} = type, origin) do
    grab_functions(origin, type, {module, identifier}, Schema.functions(module))
  end

  defp functions_for_type(type, _) do
    {type, []}
  end

  def grab_functions(origin, type, identifier, attrs) do
    {ast, type} =
      Enum.flat_map_reduce(attrs, type, fn attr, type ->
        value = Map.fetch!(type, attr)

        ast =
          quote do
            def __absinthe_function__(unquote(identifier), unquote(attr)) do
              unquote(value)
            end
          end

        ref = {:ref, origin, identifier}

        type =
          Map.update!(type, attr, fn
            value when is_list(value) ->
              [ref]

            _ ->
              ref
          end)

        {[ast], type}
      end)

    {type, ast}
  end

  @doc false
  def __ensure_middleware__([], _field, %{identifier: :subscription}) do
    [Absinthe.Middleware.PassParent]
  end

  def __ensure_middleware__([], %{identifier: identifier}, _) do
    [{Absinthe.Middleware.MapGet, identifier}]
  end

  def __ensure_middleware__(middleware, _field, _object) do
    middleware
  end
end
