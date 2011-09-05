package net.sf.royal.gui.util.tables;

import java.util.Date;

import net.sf.royal.datamodel.Author;

public class AuthorObject extends VersionableImpl{
    
    private Long id;
    private String firstName;
    private String name;
    private String nickName;
    private Date birth;
    private Date death;
    private String photo;
    private String web;
    private String story;
    private Long tekId;
    
    public AuthorObject(){}
    
    public AuthorObject(Author author){
        if (author != null){
            id = author.getId();
            firstName = author.getFirstName();
            name = author.getName();
            nickName = author.getNickName();
            birth = author.getBirth();
            death = author.getDeath();
            photo = author.getPhoto();
            web = author.getWeb();
            story = author.getStory();
            tekId = author.getTekId();
        }
    }

    /**
     * @return Returns the firstName.
     */
    public String getFirstName() {
        return firstName;
    }

    /**
     * @param firstName The firstName to set.
     */
    public void setFirstName(String firstName) {
        this.firstName = firstName;
    }

    /**
     * @return Returns the id.
     */
    public Long getId() {
        return id;
    }

    /**
     * @param id The id to set.
     */
    public void setId(Long id) {
        this.id = id;
    }

    /**
     * @return Returns the name.
     */
    public String getName() {
        return name;
    }

    /**
     * @param name The name to set.
     */
    public void setName(String name) {
        this.name = name;
    }

    /**
     * @return Returns the nickName.
     */
    public String getNickName() {
        return nickName;
    }

    /**
     * @param nickName The nickName to set.
     */
    public void setNickName(String nickName) {
        this.nickName = nickName;
    }

    /**
     * @return Returns the birth.
     */
    public Date getBirth() {
        return birth;
    }

    /**
     * @param birth The birth to set.
     */
    public void setBirth(Date birth) {
        this.birth = birth;
    }

    /**
     * @return Returns the death.
     */
    public Date getDeath() {
        return death;
    }

    /**
     * @param death The death to set.
     */
    public void setDeath(Date death) {
        this.death = death;
    }

    /**
     * @return Returns the photo.
     */
    public String getPhoto() {
        return photo;
    }

    /**
     * @param photo The photo to set.
     */
    public void setPhoto(String photo) {
        this.photo = photo;
    }
    
    
    public String toString(){
        String res = "AuthorObject :" +
                     "\n   + id : " + this.id + 
                     "\n   + nickname : " + this.nickName +
                     "\n   + name : " + this.name +
                     "\n   + firstname : " + this.firstName +
                     "\n   + photo : " + this.photo +
                     "\n   + birth : " + this.birth +
                     "\n   + death : " + this.death +
                     "\n   + web : " + this.web +
                     "\n   + story : " + this.story +
                     "\n   + version : " + this.version;
        return res;
    }

    /**
     * @return Returns the story.
     */
    public String getStory() {
        return story;
    }

    /**
     * @param story The story to set.
     */
    public void setStory(String story) {
        this.story = story;
    }

    /**
     * @return Returns the web.
     */
    public String getWeb() {
        return web;
    }

    /**
     * @param web The web to set.
     */
    public void setWeb(String web) {
        this.web = web;
    }

    public Long getTekId() {
        return tekId;
    }

    public void setTekId(Long tekId) {
        this.tekId = tekId;
    }
    
}
