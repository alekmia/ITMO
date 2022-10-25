package markup;

import java.util.List;

class Strikeout extends Marking {
    List<Markable> s;
    public Strikeout(List<Markable> listok)
    {
        this.s = listok;
    }

    @Override
    public void toMarkdown(StringBuilder str)
    {
        abstractMark(s, str, "~");
    }

    @Override
    public void toBBCode(StringBuilder str)
    {
        abstractBBCode(s, str, "[s]", "[/s]");
    }

}
