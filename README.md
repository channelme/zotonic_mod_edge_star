# zotonic_mod_edge_star

RDF-star inspired module for Zotonic enables reification of edges as resources to enable discussion and metadata about relationships.

## What is Edge Reification?

In Zotonic, edges represent relationships between resources (e.g., "Article A" → "author" → "Person B"). Sometimes you want to talk *about* these relationships dispute them, verify them, comment on them, or add metadata.

This module allows you to "reify" edges turn them into resources themselves. Once reified, you can: 

- 💬 Comment on relationships
- ⚠️ Dispute or verify connections  
- 📝 Add metadata to edges
- 🔗 Create edges pointing to other edges (meta-relationships)

This is inspired by [RDF-star (RDF 1.2)](https://www.w3.org/TR/rdf12-concepts/#section-triple-terms-reification)

## Installation

1. Add to your Zotonic site's `rebar.config` dependencies: 

```erlang
{deps, [
    {zotonic_mod_edge_star, {git, "https://github.com/channelme/zotonic_mod_edge_star.git", {branch, "main"}}}
]}.
```

2. Run:
```bash
make
```

3. Enable the module in your site's admin interface (`/admin/modules`) or via config:

```erlang
{modules, [
    ... 
    mod_edge_star
]}. 
```

4. The module will automatically create the `edge_star` database table on first run.

## Quick Start

### Basic Usage

```erlang
% Create a regular edge
{ok, EdgeId} = m_edge:insert(ArticleId, author, PersonId, Context),

% Later, someone disputes it - reify the edge
{ok, EdgeRscId} = m_edge_star:reify(EdgeId, Context),

% Now create a normal edge pointing to the reified edge
{ok, _} = m_edge:insert(UserId, disputes, EdgeRscId, Context).
```

### Using Triples

You can also work with triples directly:

```erlang
% Insert using a triple notation
{ok, EdgeId} = m_edge_star:insert(
    UserId, 
    disputes, 
    {ArticleId, author, PersonId},  % Triple
    Context
).

% This automatically: 
% 1. Finds or creates the edge (ArticleId → author → PersonId)
% 2. Reifies it as a resource
% 3. Creates the dispute edge (UserId → disputes → ReifiedEdge)
```

## API Reference

### `m_edge_star:reify/2`

Reify an edge - create a resource that represents it.

```erlang
-spec reify(EdgeId :: integer(), Context :: z:context()) -> 
    {ok, RscId :: integer()} | {error, Reason :: term()}.
```

**Example:**
```erlang
{ok, EdgeRscId} = m_edge_star:reify(123, Context).
```

If the edge is already reified, returns the existing resource ID.  The reified resource will: 
- Have category `edge_resource`
- Have a title like "Subject → predicate → Object"
- Include edge details in the body
- Be assigned to the `system_content_group`

### `m_edge_star:insert/4` and `insert/5`

Insert an edge, with automatic reification if needed.

```erlang
-spec insert(Subject, Predicate, Object, Context) -> 
    {ok, EdgeId :: integer()} | {error, Reason :: term()}.
    
Subject :: integer(),
Predicate :: atom(),
Object :: integer() | triple() | {edge, integer()},
Context :: z:context().
```

**Examples:**

```erlang
% Regular edge
{ok, EdgeId} = m_edge_star:insert(User, likes, Article, Context).

% Edge to a triple (auto-reifies)
{ok, EdgeId} = m_edge_star:insert(
    User, 
    disputes, 
    {Article, author, Person},  % Triple gets reified
    Context
).

% Edge to an edge ID (auto-reifies)
{ok, EdgeId} = m_edge_star:insert(
    Admin,
    verified,
    {edge, 456},  % Edge 456 gets reified
    Context
).

% With options
{ok, EdgeId} = m_edge_star:insert(User, disputes, Triple, [{seq, 100}], Context).
```

### `m_edge_star:get_rsc_id/2`

Get the resource ID for a reified edge.

```erlang
-spec get_rsc_id(EdgeId :: integer() | triple(), Context :: z:context()) -> 
    integer() | undefined.
```

**Example:**
```erlang
case m_edge_star:get_rsc_id(EdgeId, Context) of
    undefined -> 
        io:format("Edge not reified~n");
    RscId ->
        io:format("Edge reified as resource ~p~n", [RscId])
end.
```

### `m_edge_star:get_edge_id/2`

Get the original edge ID from a reified resource.

```erlang
-spec get_edge_id(RscId :: integer(), Context :: z:context()) -> 
    integer() | undefined.
```

**Example:**
```erlang
OriginalEdgeId = m_edge_star:get_edge_id(ReifiedRscId, Context).
```

### `m_edge_star:subjects/3` and `objects/3`

Query subjects or objects related to a triple.

```erlang
Subjects = m_edge_star:subjects({Article, author, Person}, disputes, Context).
Objects = m_edge_star:objects(UserId, disputes, Context).
```

### `m_edge_star:subject_edge_ids/3` and `object_edge_ids/3`

Get edge IDs with automatic triple expansion.

```erlang
% Returns list of {ResourceId | Triple, EdgeId} tuples
EdgeIds = m_edge_star: object_edge_ids(UserId, disputes, Context).
% Example: [{123, 456}, {{789, author, 101}, 457}]
```

## Template Usage

### Check if an edge is reified and show disputes

```django
{% with m. edge. id[article_id]. author[person_id] as edge_id %}
    {% with m.edge_star.rsc_id[edge_id] as edge_rsc %}
        {% if edge_rsc %}
            {# Show who disputes this relationship #}
            {% for user_id in m.edge. subjects[edge_rsc].disputes %}
                <div class="alert alert-warning">
                    {{ m.rsc[user_id].title }} disputes this authorship
                </div>
            {% endfor %}
            
            {# Show who verified it #}
            {% for user_id in m.edge.subjects[edge_rsc].verified %}
                <span class="badge badge-success">
                    ✓ Verified by {{ m.rsc[user_id].title }}
                </span>
            {% endfor %}
        {% endif %}
    {% endwith %}
{% endwith %}
```

### Add dispute/verify buttons

```django
{% wire id="dispute-btn" 
    postback={dispute_edge edge_id=edge_id} 
    delegate="your_module" %}
<button id="dispute-btn" class="btn btn-warning">
    Dispute this connection
</button>
```

### Model access patterns

```django
{# Get reified resource ID from edge ID #}
{{ m.edge_star.rsc_id[edge_id] }}

{# Check subjects of a reified edge #}
{% for id in m.edge_star.s[article_id][author][person_id]. disputes %}
    ... 
{% endfor %}

{# Check objects #}
{% for id in m. edge_star.o[user_id]. disputes %}
    ...
{% endfor %}
```

## Use Cases

### 1. Moderation System

```erlang
% User tags article as spam
{ok, EdgeId} = m_edge: insert(ArticleId, subject, SpamCategoryId, Context),

% Moderator flags it as incorrect
{ok, _} = m_edge_star: insert(ModeratorId, flags_incorrect, {edge, EdgeId}, Context).
```

### 2. Source Attribution & Verification

```erlang
% Someone claims ownership
{ok, EdgeId} = m_edge:insert(UserId, owns, AccountId, Context),

% Admin verifies the claim
{ok, _} = m_edge_star:insert(AdminId, verified, {edge, EdgeId}, Context).
```

### 3. Academic Citations & Disputes

```erlang
% Paper cites another paper
{ok, EdgeId} = m_edge:insert(PaperId, cites, OtherPaperId, Context),

% Researcher disputes the citation context
{ok, _} = m_edge_star:insert(ResearcherId, disputes_context, {edge, EdgeId}, Context).
```

### 4. Knowledge Graph Quality Control

```erlang
% AI suggests a relationship
{ok, EdgeId} = m_edge:insert(PersonId, born_in, CityId, [{created_by, ai_system}], Context),

% Human reviewer verifies it
{ok, _} = m_edge_star:insert(ReviewerId, verified, {edge, EdgeId}, Context).
```

## Database Schema

The module creates a simple `edge_star` table:

```sql
CREATE TABLE edge_star (
    edge_id INTEGER PRIMARY KEY REFERENCES edge(id) ON DELETE CASCADE,
    rsc_id INTEGER NOT NULL UNIQUE REFERENCES rsc(id) ON DELETE CASCADE,
    created TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now()
);
```

**One-to-one relationship**:  Each edge can have at most one reified resource.

## RDF-star Compatibility

This module is inspired by RDF-star but implements a pragmatic version:

- ✅ **Edges as objects**: You can create edges pointing to (reified) edges
- ✅ **Quoted triples**: Work with `{Subject, Predicate, Object}` tuples
- ⏸️ **RDF 1.2 alignment**: Currently follows the limitation that edges can only be objects, not subjects of other quoted triples
- 🔮 **Future extension**: The design allows for later supporting edges as subjects

## Performance Considerations

- **Caching**: `get_rsc_id/2` and `get_edge_id/2` use `z_depcache` with 1-hour TTL
- **Indexes**:  Automatic indexes on primary key and unique constraint
- **Transactions**: Reification uses database transactions for atomicity
- **Lazy reification**: Edges are only reified when needed, not automatically

## Architecture

```
Regular Edge: 
  Article --[author]--> Person

Reified Edge (after dispute):
  Article --[author]--> Person
                ↑
                |
          [Edge Resource]
                ↑
                |
            User --[disputes]--
```

The reified edge becomes a first-class resource that can: 
- Have its own edges (incoming and outgoing)
- Have comments via `mod_comment`
- Have access control via normal Zotonic ACL
- Appear in search results
- Have media attachments

## Development

### Running Tests

```bash
make test
```

### Type Checking with Dialyzer

```bash
make dialyzer
```

## Contributing

Contributions welcome! Please: 

1. Fork the repository
2. Create a feature branch
3. Make your changes with tests
4. Submit a pull request

## License

Apache License 2.0

## Author

Maas-Maarten Zeeman ([@mmzeeman](https://github.com/mmzeeman))

## Links

- [GitHub Repository](https://github.com/channelme/zotonic_mod_edge_star)
- [Zotonic CMS](https://zotonic.com)
- [RDF-star Specification](https://www.w3.org/TR/rdf12-concepts/)

## Support

- Issues: [GitHub Issues](https://github.com/channelme/zotonic_mod_edge_star/issues)
- Zotonic Community: [Zotonic Discussions](https://github.com/zotonic/zotonic/discussions)
