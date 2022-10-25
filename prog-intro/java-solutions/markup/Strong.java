package markup;


import java.util.List;

class Strong extends Marking {
    List<Markable> s;
    public Strong(List<Markable> listok)
    {
        this.s = listok;
    }

    @Override
    public void toMarkdown(StringBuilder str)
    {
        abstractMark(s, str, "__");
    }

    @Override
    public void toBBCode(StringBuilder str)
    {
        abstractBBCode(s, str, "[b]", "[/b]");
    }

}

