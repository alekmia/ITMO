package markup;


import java.util.List;

public class OrderedList extends Listing {
    List<ListItem> s;
    public OrderedList(List<ListItem> listok)
    {
        this.s = listok;
    }

    @Override
    public void toBBCode(StringBuilder str)
    {
        abstractBBCode(s, str, "[list=1]", "[/list]");
    }
}
