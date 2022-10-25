package markup;

import java.util.List;

public class UnorderedList extends Listing {
    List<ListItem> s;
    public UnorderedList(List<ListItem> listok)
    {
        this.s = listok;
    }

    @Override
    public void toBBCode(StringBuilder str)
    {
        abstractBBCode(s, str, "[list]", "[/list]");
    }
}
