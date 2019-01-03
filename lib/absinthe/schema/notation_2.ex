defmodule Absinthe.Schema.Notation2 do
  alias Absinthe.Blueprint.Schema
  alias Absinthe.Utils

  Module.register_attribute(__MODULE__, :placement, accumulate: true)

  defmacro __using__(_opts) do
    quote location: :keep do
      import Absinthe.Schema.Notation2, only: [schema: 1]
    end
  end

  ### Macro API ###

  defmacro schema(do: block) do
    quote location: :keep do
      Module.register_attribute(__MODULE__, :absinthe_blueprint, accumulate: true)
      Module.register_attribute(__MODULE__, :absinthe_desc, accumulate: true)

      Module.register_attribute(__MODULE__, :__absinthe_field_config__, accumulate: false)

      Module.register_attribute(__MODULE__, :__absinthe_enums__, accumulate: true)
      Module.register_attribute(__MODULE__, :__absinthe_enum_directives__, accumulate: true)
      Module.register_attribute(__MODULE__, :__absinthe_enum_values__, accumulate: true)
      Module.register_attribute(__MODULE__, :__absinthe_fields__, accumulate: true)
      Module.register_attribute(__MODULE__, :__absinthe_field_args__, accumulate: true)
      Module.register_attribute(__MODULE__, :__absinthe_import_fields__, accumulate: true)
      Module.register_attribute(__MODULE__, :__absinthe_input_object_types__, accumulate: true)
      Module.register_attribute(__MODULE__, :__absinthe_interfaces__, accumulate: true)
      Module.register_attribute(__MODULE__, :__absinthe_locations__, accumulate: true)
      Module.register_attribute(__MODULE__, :__absinthe_objects__, accumulate: true)
      Module.register_attribute(__MODULE__, :__absinthe_scalar_types__, accumulate: true)
      Module.register_attribute(__MODULE__, :__absinthe_sdl_definitions__, accumulate: true)
      Module.register_attribute(__MODULE__, :__absinthe_subscription_triggers__, accumulate: true)
      Module.register_attribute(__MODULE__, :__absinthe_type_imports__, accumulate: true)

      import Absinthe.Resolution.Helpers,
        only: [
          async: 1,
          async: 2,
          batch: 3,
          batch: 4
        ]

      import Absinthe.Schema.Notation2
      unquote(block)
      import Absinthe.Schema.Notation2, only: []

      def __absinthe_blueprint__() do
         x = %Absinthe.Blueprint{
          schema_definitions: [
            %Schema.SchemaDefinition{
              imports: @__absinthe_type_imports__,
              module: __MODULE__,
              __private__: [],
              __reference__: %{
                module: __MODULE__,
                location: %{
                  file: __ENV__.file,
                  line: __ENV__.line
                }
              },
              type_definitions: @__absinthe_objects__ ++ @__absinthe_sdl_definitions__ ++ @__absinthe_enums__ ++ @__absinthe_scalar_types__ ++ @__absinthe_input_object_types__
            },
          ]
        } # |> IO.inspect(limit: :infinity)

        x
      end
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
  defmacro config(config_function) do
    quote location: :keep do
      Absinthe.Schema.Notation2.__config__(__MODULE__, unquote(config_function))
    end
  end

  @doc false
  def __config__(module, function) do
    Module.put_attribute(module, :__absinthe_field_config__, function)
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
    quote location: :keep do
      Absinthe.Schema.Notation2.__trigger__(
        __MODULE__,
        unquote(mutations),
        unquote(attrs)
      )
    end
  end

  @doc false
  def __trigger__(module, mutations, attrs) do
    mutations
    |> List.wrap()
    |> Enum.each(fn trigger ->
      Module.put_attribute(module, :__absinthe_subscription_triggers__, {trigger, attrs})
    end)
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
  defmacro object(identifier, attrs \\ [], block)

  defmacro object(identifier, attrs, do: block) do
    quote location: :keep do
      Module.delete_attribute(__MODULE__, :__absinthe_fields__)
      Module.delete_attribute(__MODULE__, :__absinthe_interfaces__)

      @desc nil

      if meta = Keyword.get(unquote(attrs), :meta) do
        meta meta
      end

      unquote(block)

      if @desc do
        @absinthe_desc @desc
      end

      Absinthe.Schema.Notation2.__object__(
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

  @doc false
  def __object__(module, file, line, identifier, attrs, fields, interfaces) do
    reference = build_reference(module, file, line)

    attrs =
      attrs
      # |> handle_deprecate()
      |> Keyword.put(:__reference__, reference)
      |> Keyword.put(:identifier, identifier)
      |> Keyword.put(:module, module)
      |> Keyword.put(:interfaces, interfaces)
      |> Keyword.put(:fields, fields)
      |> Keyword.put_new(:name, default_name(Schema.ObjectTypeDefinition, identifier))

    object = struct!(Schema.ObjectTypeDefinition, attrs)

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
    quote location: :keep do
      unquote(block)

      Absinthe.Schema.Notation2.__interface__(
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
    quote location: :keep do
      interface unquote(identifier) do
        :ok
      end
    end
  end

  @doc false
  def __interface__(module, identifier) do
      # |> record!(Schema.InterfaceTypeDefinition, identifier, attrs)
    Module.put_attribute(module, :__absinthe_interfaces__, identifier)
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
    quote location: :keep do
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
    quote location: :keep do
      Absinthe.Schema.Notation2.__deprecate__(__MODULE__, unquote(msg))
    end
  end

  @doc false
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
    quote location: :keep do
      Absinthe.Schema.Notation2.__resolve_type__(__MODULE__, unquote(func_ast))
    end
  end

  @doc false
  def __resolve_type__(module, function_ast) do
    Module.put_attribute(module, :resolve_type, function_ast)
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

  defmacro field(identifier, do: block) do
    quote location: :keep do
      field unquote(identifier), [] do
        unquote(block)
      end
    end
  end

  defmacro field(identifier, attrs) do
    quote location: :keep, bind_quoted: [identifier: identifier, attrs: attrs] do
      attrs =
        if is_list(attrs) do
          attrs
        else
          [type: attrs]
        end

      field identifier, attrs do
        :ok
      end
    end
  end

  @doc """
  Defines a GraphQL field

  See `field/4`
  """
  defmacro field(identifier, attrs, do: block) do
    quote location: :keep do
      identifier = unquote(identifier)
      attrs = unquote(attrs)

      attrs =
        if is_list(attrs) do
          attrs
        else
          [type: attrs]
        end

      Module.delete_attribute(__MODULE__, :__absinthe_field_args__)
      @__absinthe_field_config__ nil

      for {arg_identifier, arg_attrs} <- Keyword.get(attrs, :args, []) do
        arg arg_identifier, arg_attrs
      end

      @desc nil

      unquote(block)

      if @__absinthe_field_config__ do
        def __absinthe_function__(identifier, :ok) do
          @__absinthe_field_config__
        end

        Module.put_attribute(__MODULE__, :__absinthe_field_config__, {:ref, __MODULE__, {Absinthe.Blueprint.Schema.FieldDefinition, {:query, identifier}}})
      end

      if @desc do
        @absinthe_desc @desc
      end

      {resolve, attrs} = Keyword.pop(attrs, :resolve)

      if resolve do
        resolve resolve
      end

      Absinthe.Schema.Notation2.__field__(
        __MODULE__,
        __ENV__.file,
        __ENV__.line,
        identifier,
        attrs,
        @__absinthe_field_config__,
        @__absinthe_field_args__
      )
    end
  end

  defmacro field(identifier, type, attrs) do
    quote location: :keep do
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
    quote location: :keep do
      field unquote(identifier), Keyword.put(unquote(attrs), :type, unquote(type)) do
        unquote(block)
      end
    end
  end

  @doc false
  def __field__(module, file, line, identifier, attrs, config, args) do
    reference = build_reference(module, file, line)

    attrs =
      attrs
      |> Keyword.delete(:args)
      |> handle_deprecate
      |> Keyword.put(:__reference__, reference)
      |> Keyword.put(:identifier, identifier)
      |> Keyword.put(:module, module)
      |> Keyword.put(:config, config)
      |> Keyword.put(:arguments, args)
      |> Keyword.put_new(:name, default_name(Schema.FieldDefinition, identifier))

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
    quote location: :keep do
      middleware Absinthe.Resolution, unquote(func_ast)
    end
  end

  @placement {:complexity, [under: [:field]]}
  defmacro complexity(complexity) do
    quote location: :keep do
      Absinthe.Schema.Notation2.__complexity__(__MODULE__, unquote(complexity))
    end
  end

  @doc false
  def __complexity__(module, complexity) do
    Module.put_attribute(module, :__absinthe_complexity__, complexity)
  end

  @placement {:middleware, [under: [:field]]}
  defmacro middleware(middleware, opts \\ []) do
    quote location: :keep do
      Absinthe.Schema.Notation2.__middleware__(
        __MODULE__,
        unquote(middleware),
        unquote(opts)
      )
    end
  end

  @doc false
  def __middleware__(module, middleware, opts) do
    new_middleware =
      case middleware do
        {module, fun} ->
          {:{}, [], [{module, fun}, opts]}

        atom when is_atom(atom) ->
          case Atom.to_string(atom) do
            "Elixir." <> _ ->
              {:{}, [], [{atom, :call}, opts]}

            _ ->
              {:{}, [], [{module, atom}, opts]}
          end

        val ->
          val
      end

    Module.put_attribute(module, :middleware, new_middleware)
  end

  @placement {:is_type_of, [under: [:object]]}
  @doc """

  ## Placement

  #{Utils.placement_docs(@placement)}
  """
  defmacro is_type_of(func_ast) do
    quote location: :keep do
      __is_type_of__(__MODULE__, unquote(func_ast))
    end
  end

  @doc false
  def __is_type_of__(module, func_ast) do
    Module.put_attribute(module, :is_type_of, func_ast)
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
  defmacro arg(identifier, attrs) do
    quote location: :keep, bind_quoted: [attrs: attrs, identifier: identifier] do
      Absinthe.Schema.Notation2.__arg__(
        __MODULE__,
        __ENV__.file,
        __ENV__.line,
        identifier,
        attrs
      )
    end
  end

  @doc false
  def __arg__(module, file, line, identifier, attrs) when is_list(attrs) do
    reference = build_reference(module, file, line)

    attrs =
      attrs
      |> handle_deprecate()
      |> Keyword.put(:identifier, identifier)
      |> Keyword.put(:module, module)
      |> Keyword.put(:name, to_string(identifier))
      |> Keyword.put(:__reference__, reference)

    arg = struct!(Schema.InputValueDefinition, attrs)
    Module.put_attribute(module, :__absinthe_field_args__, arg)
  end

  def __arg__(module, file, line, identifier, type) do
    __arg__(module, file, line, identifier, [type: type])
  end

  # SCALARS

  @doc """
  Defines a scalar type

  See `scalar/3`
  """
  defmacro scalar(identifier, do: block) do
    quote location: :keep do
      scalar unquote(identifier), [] do
        unquote(block)
      end
    end
  end

  defmacro scalar(identifier, attrs) do
    quote location: :keep do
      scalar unquote(identifier), unquote(attrs) do
        :ok
      end
    end
  end

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
  defmacro scalar(identifier, attrs, do: block) do
    quote location: :keep do
      unquote(block)

      Absinthe.Schema.Notation2.__scalar__(
        __MODULE__,
        __ENV__.file,
        __ENV__.line,
        unquote(identifier),
        unquote(attrs)
      )
    end
  end

  @doc false
  def __scalar__(module, file, line, identifier, attrs) do
    reference = build_reference(module, file, line)

    attrs =
      attrs
      |> Keyword.put(:identifier, identifier)
      |> Keyword.put(:__reference__, reference)
      |> Keyword.put(:module, module)
      |> Keyword.put_new(:name, default_name(identifier, Schema.ScalarTypeDefinition))

    scalar = struct!(Schema.ScalarTypeDefinition, attrs)
    Module.put_attribute(module, :__absinthe_scalar_types__, scalar)
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
    quote location: :keep do
      Absinthe.Schema.Notation2.__serialize__(__MODULE__, unquote(func_ast))
    end
  end

  @doc false
  def __serialize__(module, fun_ast) do
    Module.put_attribute(module, :serialize, fun_ast)
  end

  @placement {:private,
              [under: [:field, :object, :input_object, :enum, :scalar, :interface, :union]]}
  @doc false
  defmacro private(owner, key, value) do
    quote location: :keep do
      Absinthe.Schema.Notation2.__private__(__MODULE__, unquote(owner), [
        {unquote(key), unquote(value)}
      ])
    end
  end

  @doc false
  def __private__(module, owner, keyword_list) when is_list(keyword_list) do
    Module.put_attribute(module, :__private__, [{owner, keyword_list}])
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
    quote location: :keep do
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
    quote location: :keep do
      Absinthe.Schema.Notation2.__private__(__MODULE__, :meta, unquote(keyword_list))
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
    quote location: :keep do
      Absinthe.Schema.Notation2.__parse__(__MODULE__, unquote(func_ast))
    end
  end

  @doc false
  def __parse__(module, fun_ast) do
    Module.put_attribute(module, :parse, fun_ast)
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
    quote location: :keep do
      Module.delete_attribute(__MODULE__, :__absinthe_args__)
      Module.delete_attribute(__MODULE__, :__absinthe_directive_locations__)
      @__absinthe_expand__ nil

      @desc nil

      unquote(block)

      if @desc do
        @absinthe_desc @desc
      end

      Absinthe.Schema.Notation2.__directive__(
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
  def __directive__(module, file, line, identifier, attrs, args, locations, expand) do
    reference = build_reference(module, file, line)

    attrs =
      attrs
      |> Keyword.put(:args, args)
      |> Keyword.put(:expand, expand)
      |> Keyword.put(:identifier, identifier)
      |> Keyword.put(:locations, locations)
      |> Keyword.put(:module, module)
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
    quote location: :keep do
      Absinthe.Schema.Notation2.__locations__(module, unquote(ast_node))
    end
  end

  @doc false
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
    quote location: :keep do
      Absinthe.Schema.Notation2.__expand__(__MODULE__, unquote(func_ast))
    end
  end

  @doc false
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
    quote location: :keep do
      Module.delete_attribute(__MODULE__, :__absinthe_fields__)

      @desc nil

      unquote(block)

      if @desc do
        @absinthe_desc @desc
      end

      Absinthe.Schema.Notation2.__input_object__(
        __MODULE__,
        __ENV__.file,
        __ENV__.line,
        unquote(identifier),
        unquote(attrs),
        @__absinthe_fields__
      )
    end
  end

  @doc false
  def __input_object__(module, file, line, identifier, attrs, fields) do
    reference = build_reference(module, file, line)

    attrs =
      attrs
      |> Keyword.put(:identifier, identifier)
      |> Keyword.put(:module, module)
      |> Keyword.put(:fields, fields)
      |> Keyword.put(:__reference__, reference)
      |> Keyword.put_new(:name, default_name(identifier, Schema.InputObjectTypeDefinition))

    input_object = struct!(Schema.InputObjectTypeDefinition, attrs)
    Module.put_attribute(module, :__absinthe_input_object_types__, input_object)
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
    quote location: :keep do
      Module.delete_attribute(__MODULE__, :__absinthe_union_types__)
      Module.delete_attribute(__MODULE__, :__absinthe_resolve_type__)

      @desc nil

      unquote(block)

      if @desc do
        @absinthe_desc @desc
      end

      Absinthe.Schema.Notation2.__union__(
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

  @doc false
  def __union__(module, file, line, identifier, attrs, description, types, resolve_type) do
    reference = build_reference(module, file, line)

    attrs =
      attrs
      |> Keyword.put(:__reference__, reference)
      |> Keyword.put(:description, description)
      |> Keyword.put(:identifier, identifier)
      |> Keyword.put(:types, types)
      |> Keyword.put(:resolve_type, resolve_type)
      |> Keyword.put_new(:name, default_name(Absinthe.Type.Union, identifier))

    union = struct!(Absinthe.Type.Union, attrs)
    Module.put_attribute(module, :__absinthe_unions__, union)
  end

  @placement {:types, [under: [:union]]}
  @doc """
  Defines the types possible under a union type

  See `union/3`

  ## Placement

  #{Utils.placement_docs(@placement)}
  """
  defmacro types(types) do
    quote location: :keep do
      Absinthe.Schema.Notation2.__types__(__MODULE__, unquote(types))
    end
  end

  @doc false
  def __types__(module, types) do
    Module.put_attribute(module, :__absinthe_union_types__, types)
  end

  # ENUMS

  @doc """
  Defines an enum type

  See `enum/3`
  """
  defmacro enum(identifier, do: block) do
    quote location: :keep do
      enum unquote(identifier), [] do
        unquote(block)
      end
    end
  end

  defmacro enum(identifier, attrs) do
    quote location: :keep do
      enum unquote(identifier), Keyword.delete(unquote(attrs), :values) do
        for attr <- Keyword.get(unquote(attrs), :values) do
          value attr
        end
      end
    end
  end

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
  defmacro enum(identifier, attrs, do: block) do
    quote location: :keep do
      Module.delete_attribute(__MODULE__, :__absinthe_enum_values__)
      Module.delete_attribute(__MODULE__, :__absinthe_enum_directives__)

      @desc nil

      unquote(block)

      if @desc do
        @absinthe_desc @desc
      end

      Absinthe.Schema.Notation2.__enum__(
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

  @doc false
  def __enum__(module, file, line, identifier, attrs, values, directives) do
    reference = build_reference(module, file, line)

    attrs =
      attrs
      |> Keyword.put(:__reference__, reference)
      |> Keyword.put(:directives, directives)
      |> Keyword.put(:identifier, identifier)
      |> Keyword.put(:module, module)
      |> Keyword.put(:values, values)
      |> Keyword.put_new(:name, default_name(Schema.EnumTypeDefinition, identifier))

    enum = struct!(Schema.EnumTypeDefinition, attrs)
    Module.put_attribute(module, :__absinthe_enums__, enum)
  end

  defmacro values(values) do
    quote location: :keep do
      for value <- unquote(values) do
        value value
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
    quote location: :keep do
      Absinthe.Schema.Notation2.__value__(
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

    default_name =
      identifier
      |> to_string()
      |> String.upcase()

    attrs =
      raw_attrs
      |> Keyword.put(:__reference__, reference)
      |> Keyword.put(:identifier, identifier)
      |> Keyword.put(:module, module)
      |> Keyword.put(:value, Keyword.get(raw_attrs, :as, identifier))
      |> Keyword.put_new(:name, default_name)
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
    quote location: :keep do
      Absinthe.Schema.Notation2.__description__(__MODULE__, unquote(text))
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
    quote location: :keep do
      %Absinthe.Blueprint.TypeReference.NonNull{of_type: unquote(type)}
    end
  end

  @doc """
  Marks a type reference as a list of the given type

  See `field/3` for examples
  """
  defmacro list_of(type) do
    quote location: :keep do
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
    quote location: :keep do
      Absinthe.Schema.Notation2.__import_fields__(
        __MODULE__,
        unquote(source_criteria),
        unquote(opts)
      )
    end
  end

  @doc false
  def __import_fields__(module, source_criteria, opts) do
    Module.put_attribute(module, :__absinthe_import_fields__, {source_criteria, opts})
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
  defmacro import_types(type_module, opts \\ []) do
    quote location: :keep do
      Absinthe.Schema.Notation2.__import_types__(__MODULE__, unquote(type_module), unquote(opts))
    end
  end

  @doc false
  def __import_types__(module, type_module, opts) do
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

  # ------------------------------

  defp build_reference(module, file, line) do
    %{
      module: module,
      location: %{
        file: file,
        line: line
      }
    }
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
  defp do_import_sdl(_env, nil, opts) do
    case Keyword.fetch(opts, :path) do
      {:ok, path} ->
        quote location: :keep do
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
    ref = build_reference(env.module, env.file, env.line)

    quote location: :keep do
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
