package markup;

import java.util.List;

class Emphasis extends Marking {
    List<Markable> s;
    public Emphasis(List<Markable> listok)
    {
        this.s = listok;
    }

    @Override
    public void toMarkdown(StringBuilder str)
    {
        abstractMark(s, str, "*");
    }

    @Override
    public void toBBCode(StringBuilder str)
    {
        abstractBBCode(s, str, "[i]", "[/i]");
    }

}
