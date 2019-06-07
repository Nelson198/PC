import static javax.swing.JOptionPane.*;
import javax.swing.JPasswordField;

void login(String username, String password) {
  if (password == null)
    return;

  toSocket.println("Login:" + username + ":" + password);
  toSocket.flush();

  try {
    String answer = fromSocket.readLine();
    loginMenu.set(false);
    messageMenu.set(true);
    if (answer.equals("LoginOk")) {
      message = "Bem-vindo \"" + username + "\"!\nIniciaste sessão com sucesso!";
      success.set(true);
      onlineMenu.set(true);
    } else if (answer.equals("LoginInexistentUser")) {
      message = "Conta inexistente. Registe-se primeiro.";
      usrText = "";
      success.set(false);
    } else if (answer.equals("LoginAlreadyLoggedIn")) {
      message = "O utilizador indicado já tem sessão iniciada!";
      usrText = "";
      success.set(false);
    } else if (answer.equals("LoginWrongPassword")) {
      message = "Password incorreta!";
      usrText = "";
      success.set(false);
    }
  } catch (Exception e) {}

  pwdText = "";
  usr.set(true);
  pwd.set(false);
}

void register(String username, String password) {
    if (password == null)
      return;

    toSocket.println("Register:" + username + ":" + password);
    toSocket.flush();

    try {
      String answer = fromSocket.readLine();
      registerMenu.set(false);
      messageMenu.set(true);
      if (answer.equals("RegisterOk")) {
        message = "Bem-vindo \"" + username + "\"!\nRegistaste-te com sucesso!";
        success.set(true);
        onlineMenu.set(true);
      } else if (answer.equals("RegisterUserExists")) {
        message = "Já existe um utilizador com esse nome!";
        usrText = "";
        success.set(false);
      }
    } catch (Exception e) {}
  
  pwdText = "";
  usr.set(true);
  pwd.set(false);
}

void unregister(String username, String password) {
    if (password == null)
      return;

    toSocket.println("Unregister:" + username + ":" + password);
    toSocket.flush();

    try {
      String answer = fromSocket.readLine();
      unregisterMenu.set(false);
      messageMenu.set(true);
      if (answer.equals("UnregisterOk")) {
        message = "Lamentamos que queira apagar a conta \"" + username + "\"!";
        success.set(true);
        mainMenu.set(true);
        switchMusic(0);
      } else if (answer.equals("UnregisterUserNotExists")) {
        message = "Não existe um utilizador com esse nome!";
        success.set(false);
      } else if (answer.equals("UnregisterWrongPassword")) {
        message = "Password incorreta!";
        success.set(false);
      }
    } catch (Exception e) {}
  usrText = "";
  pwdText = "";
  usr.set(true);
  pwd.set(false);
}
