namespace RayTracer
{
    public static class Program
    {
        public static void Main(string[] args)
        {
            var scene = Samples.randomScene();

            using var window = Window.ForScene(scene);
            window.Run();
        }
    }
}
