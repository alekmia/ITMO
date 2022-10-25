package markup;

import java.util.List;

public class ListItem implements Listable {
    List<Listing> s;
    public ListItem(List<Listing> listok)
    {
        this.s = listok;
    }

    @Override
    public void toBBCode(StringBuilder str)
    {
        str.append("[*]");
        for(Listing pi : s)
            pi.toBBCode(str);
    }
}
