{% with edge.id as id %}
{% if m.edge_star[id].rsc_id as reified_edge %}
    <a id="{{ #reify.id }}" href="{% url admin_edit_rsc id=reified_edge  %}" class="btn btn-info">{_ Edit resource _}</a>
{% else %}
    <button id="{{ #reify.id }}" class="btn btn-default">{_ Make edge resource _}</button>
    {% wire id=#reify.id
            postback={reify_edge edge=id replace_button} 
            delegate="mod_edge_star"
    %}
{% endif %}
{% endwith %}
